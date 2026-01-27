%%%-------------------------------------------------------------------
%%% @author crazy
%%%
%%% Auction Monitor (Load Balancer).
%%% 1. Checks for free slots (Max 3 concurrent auctions).
%%% 2. Fetches pending auctions from Java via RPC.
%%% 3. Spawns auction processes and monitors them.
%%% 4. Cleans up slots when auctions finish or crash.
%%% @end
%%%-------------------------------------------------------------------
-module('DS_auction_monitor').
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% GenServer Callbacks, default, these callback are necessary bc of the framework
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%note that the private functions are not exported, helper and internal
%CONFIGURATION
-define(JAVA_NODE, 'java_node@MSI').      %name of the Java Node to communicate
-define(JAVA_MAILBOX, 'java_listener').         %name of the Mailbox created in Java
-define(POLL_INTERVAL, 5000).                   %how often to check for new auctions (ms)


%structure of the internal state of the server.
-record(state, {
  active_slots = #{} :: map() %map of active process that will be populated
}).

%%Starts the server and registers it locally. %like in the monitor
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%saver process with its local name and call backs are ion this module

%% ===================================================================
%% GEN_SERVER CALLBACKS
%% ===================================================================

%%Initializes the server. Starts the periodic timer.
init([]) ->
  io:format("[MONITOR] Server started. Waiting for Java...~n"),
  %start the periodic timer. It sends 'check_for_auctions' to self() every 5s.
  timer:send_interval(?POLL_INTERVAL, check_for_auctions), %message sent like an atom
  {ok, #state{active_slots = #{}}}. %ok mess, at th ebaginning empty map

%when the atom is received : Checks availability and fetches auctions.
handle_info(check_for_auctions, State) ->
  CurrentActive = maps:size(State#state.active_slots),
  MaxSlots = 3, %check active auctionsn with the max of 3

  if %if less than 3 needs to search for a new auction in the DB or how many are needed
    CurrentActive < MaxSlots ->
      %%Calculate how many auctions we can take.
      SlotsAvailable = MaxSlots - CurrentActive,
      io:format("[MONITOR] Slots free: ~p. Requesting auctions from Java...~n", [SlotsAvailable]),

      %% 1. Ask Java for new auctions (RPC Call), call the private function
      NewAuctions = fetch_auctions_rpc(SlotsAvailable), %call this function

      %% 2. Start the received auctions from java
      NewState = start_auctions_batch(NewAuctions, State),
      {noreply, NewState}; %use new state

    true ->
      %if slots are ok, do nothing.
      io:format("[MONITOR] All slots full (~p/~p). Skipping request.~n", [CurrentActive, MaxSlots]),
      {noreply, State}
  end;

%%Handles the death of an auction process.
%% This message is sent automatically by Erlang when a monitored process dies.
handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
  ActiveSlots = State#state.active_slots, %PID = who dies, reason is why(normal or error)

  %% Check if the dead process was one of our auctions
  case maps:find(Pid, ActiveSlots) of
    {ok, AuctionId} ->
      io:format("[MONITOR] Auction ~p ended/crashed via ~p. Slot freed.~n", [AuctionId, Reason]),

      %% Remove the PID from the map to free up a slot
      NewSlots = maps:remove(Pid, ActiveSlots),
      {noreply, State#state{active_slots = NewSlots}};

    error ->
      %% We received a DOWN message from a process we don't know (should not happen)
      {noreply, State}
  end;

%% Handle unexpected messages (logging purposes)
handle_info(Info, State) ->
  io:format("[MONITOR] Unexpected message: ~p~n", [Info]),
  {noreply, State}.

%% Unused callbacks, necessary by default
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ===================================================================
%% INTERNAL HELPER FUNCTIONS, private
%% ===================================================================

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
      io:format("[MONITOR] Received ~p auctions from Java.~n", [length(AuctionList)]),
      AuctionList;

    {MsgId, java_error, Reason} ->
      io:format("[MONITOR] Java reported error: ~p~n", [Reason]),
      []; %% Return empty list on error

    _Other ->
      %% Ignore messages that don't match our MsgId (garbage collection)
      []
  after 2000 ->
    %% Timeout if Java is too slow or down
    io:format("[MONITOR] TIMEOUT: Java is not responding.~n"),
    []
  end.

%% Iterates over the list of auctions and starts them one by one.
start_auctions_batch([], State) ->
  State; %% Base case: No more auctions to start

start_auctions_batch([AuctionData | Rest], State) ->
  %% Parse the AuctionData tuple received from Java
  %% Expecting format: {Id, Name, Price}
  {AuctionId, Name, Price} = AuctionData,

  io:format("[MONITOR] Spawning Auction: ID=~p, Item=~s~n", [AuctionId, Name]),

  %% 1. Spawn the auction process
  %% We spawn a function that calls the handler's start function.
  %% IMPORTANT: Ensure DS_auction_handler:start/4 exists and matches arguments!
  Pid = spawn(fun() -> 'DS_auction_handler':start(AuctionId, Price, Name, 60) end),

  %% 2. MONITOR the process
  %% This ensures we get a 'DOWN' message when it finishes or crashes.
  erlang:monitor(process, Pid),

  %% 3. Update the state map (PID -> ID)
  NewSlots = maps:put(Pid, AuctionId, State#state.active_slots),

  %% 4. Recursively process the rest of the list
  start_auctions_batch(Rest, State#state{active_slots = NewSlots}).