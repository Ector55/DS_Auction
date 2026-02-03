%%%-------------------------------------------------------------------
%%% @author crazy
%%%-------------------------------------------------------------------
-module('DS_auction_manager').
-author("crazy").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% GenServer Callbacks, default, these callback are necessary bc of the framework
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%note that the private functions are not exported, helper and internal

-define(JAVA_NODE, 'java_node@127.0.0.1').
-define(JAVA_MAILBOX, 'java_listener').         %name of the Mailbox created in Java
-define(POLL_INTERVAL, 5000).                   %how often to check for new auctions (ms)

%structure of the internal state of the server.
-record(state, {
  active_slots = #{} :: map() %map of active process that will be populated
}).

%%Starts the server and registers it locally.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%saver process with its local name and call backs are ion this module

%genn server callbacks

%%Initializes the server. Starts the periodic timer.
init([]) ->
  io:format("[MANAGER] Server started. Waiting for Java...~n"),

  InitialAuctions = fetch_auctions_rpc(3),
  InitialState = #state{active_slots = #{}},
  NewState = start_auctions_batch(InitialAuctions, InitialState),
  %start the periodic timer. It sends 'check_for_auctions' to self() every 5s.
  timer:send_interval(?POLL_INTERVAL, check_for_auctions), %message sent like an atom
  {ok, NewState}.

%when the atom is received : Checks availability and fetches auctions.
handle_info(check_for_auctions, State) ->
  CurrentActive = maps:size(State#state.active_slots),
  MaxSlots = 3, %check active auctionsn with the max of 3

  if %if less than 3 needs to search for a new auction in the DB or how many are needed
    CurrentActive < MaxSlots ->
      %%Calculate how many auctions we can take.
      SlotsAvailable = MaxSlots - CurrentActive,
      io:format("[MANAGER] Slots free: ~p. Requesting auctions from Java...~n", [SlotsAvailable]),

      %% 1. Ask Java for new auctions (RPC Call), call the private function
      NewAuctions = fetch_auctions_rpc(SlotsAvailable), %call this function

      %% 2. Start the received auctions from java
      NewState = start_auctions_batch(NewAuctions, State),
      {noreply, NewState}; %use new state

    true ->
      %if slots are ok, do nothing.
      io:format("[MANAGER] All slots full (~p/~p). Skipping request.~n", [CurrentActive, MaxSlots]),
      {noreply, State}
  end;

%%Handles the death of an auction process.
%% This message is sent automatically by Erlang when a monitored process dies.
handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
  ActiveSlots = State#state.active_slots, %PID = who dies, reason is why(normal or error)

  %% Check if the dead process was one of our auctions
  case maps:find(Pid, ActiveSlots) of
    {ok, AuctionId} ->
      io:format("[MANAGER] Auction ~p ended/crashed via ~p. Slot freed.~n", [AuctionId, Reason]),

      %% Remove the PID from the map to free up a slot
      NewSlots = maps:remove(Pid, ActiveSlots),
      {noreply, State#state{active_slots = NewSlots}};

    error ->
      %% We received a DOWN message from a process we don't know (should not happen)
      {noreply, State}
  end;

%% Handle unexpected messages (logging purposes)
handle_info(Info, State) ->
  io:format("[MANAGER] Unexpected message: ~p~n", [Info]),
  {noreply, State}.

%% Unused callbacks, necessary by default
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Communicates with Java to fetch Pending auctions.
%% Protocol: Sends {self(), MsgId, get_next_auctions, Count}
fetch_auctions_rpc(Count) ->
  MsgId = make_ref(), %% Create a unique ID for this specific request

  %% Send message to the Java Mailbox
  %% Format: {SenderPID, RequestID, Command, Argument}
  {?JAVA_MAILBOX, ?JAVA_NODE} ! {self(), MsgId, get_next_auctions, Count}, %{who, IDreq, comando, arg}

  %% Wait for the reply
  receive
    {MsgId, java_response, AuctionList} ->
      io:format("[MANAGER] Received ~p auctions from Java.~n", [length(AuctionList)]),
      AuctionList;

    {MsgId, java_error, Reason} ->
      io:format("[MANAGER] Java reported error: ~p~n", [Reason]),
      []; %% Return empty list on error

    _Other ->
      %% Ignore messages that don't match our MsgId (garbage collection)
      []
  after 2000 ->
    %% Timeout if Java is too slow or down
    io:format("[MANAGER] TIMEOUT: Java is not responding.~n"),
    []
  end.

%% Iterates over the list of auctions and starts them one by one.
start_auctions_batch([], State) ->
  State; %% Base case: No more auctions to start

start_auctions_batch([AuctionData | Rest], State) ->
  {AuctionId, Name, Price} = AuctionData,
  io:format("[MANAGER] Spawning Auction: ID=~p, Item=~s~n", [AuctionId, Name]),

  %% 1. Spawn the auction process
  Pid = spawn(fun() -> 'DS_auction_handler':start(AuctionId, Price, Name, 300) end),

  %% 2. DELEGATE monitoring to the specialized Monitor module
  %% Remove the old: erlang:monitor(process, Pid),
  'DS_auction_monitor':monitor_auction(Pid, AuctionId),

  %% 3. Update the Manager state map (PID -> ID)
  %% This is still necessary so the Manager knows when a slot is free via handle_info
  NewSlots = maps:put(Pid, AuctionId, State#state.active_slots),

  start_auctions_batch(Rest, State#state{active_slots = NewSlots}).