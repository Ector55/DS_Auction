%%%-------------------------------------------------------------------
%%% @author crazy
%%% @doc
%%% Auction Manager - handles the lifecycle of auction slots and
%%% coordinates between Java and Erlang handlers.
%%%-------------------------------------------------------------------
-module('DS_auction_manager').
-author("crazy").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% GenServer Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(JAVA_NODE, 'java_node@127.0.0.1').
-define(JAVA_MAILBOX, 'java_listener').         %% Name of the Mailbox created in Java
-define(POLL_INTERVAL, 5000).                   %% Check for new auctions every 5s

%% Internal state structure
-record(state, {
  active_slots = #{} :: map() %% Map of PID -> AuctionId
}).

%% Starts the manager server and registers it locally
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Initializes the server and starts the periodic polling timer
init([]) ->
  io:format("[MANAGER] Server started. Waiting for Java...~n"),

  InitialAuctions = fetch_auctions_rpc(3),
  InitialState = #state{active_slots = #{}},
  NewState = start_auctions_batch(InitialAuctions, InitialState),

  %% Sends 'check_for_auctions' to self every 5 seconds
  timer:send_interval(?POLL_INTERVAL, check_for_auctions),
  {ok, NewState}.

%% Checks availability and requests new auctions from Java
handle_info(check_for_auctions, State) ->
  CurrentActive = maps:size(State#state.active_slots),
  MaxSlots = 3,

  if
    CurrentActive < MaxSlots ->
      SlotsAvailable = MaxSlots - CurrentActive,
      io:format("[MANAGER] Slots free: ~p. Requesting auctions from Java...~n", [SlotsAvailable]),

      %% 1. Fetch new auctions via RPC
      NewAuctions = fetch_auctions_rpc(SlotsAvailable),

      %% 2. Start the handlers for the new batch
      NewState = start_auctions_batch(NewAuctions, State),
      {noreply, NewState};

    true ->
      io:format("[MANAGER] All slots full (~p/~p). Skipping request.~n", [CurrentActive, MaxSlots]),
      {noreply, State}
  end;

handle_info({auction_ended, AuctionId, _Winner, _Price}, State) ->
  io:format("[MANAGER] Auction ~p notified end. Waiting for process cleanup...~n", [AuctionId]),
  {noreply, State};

%% This handles the ACTUAL slot liberation
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
  ActiveSlots = State#state.active_slots,
  case maps:find(Pid, ActiveSlots) of
    {ok, AuctionId} ->
      io:format("[MANAGER] Freeing slot for Auction ~p. Requesting new item...~n", [AuctionId]),
      NewSlots = maps:remove(Pid, ActiveSlots),

      %% This triggers the immediate fetch from Java
      self() ! check_for_auctions,

      {noreply, State#state{active_slots = NewSlots}};
    error ->
      {noreply, State}
  end;

handle_info(Info, State) ->
  io:format("[MANAGER] Unexpected message: ~p~n", [Info]),
  {noreply, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Communicates with Java to fetch Pending auctions
fetch_auctions_rpc(Count) ->
  MsgId = make_ref(),

  {?JAVA_MAILBOX, ?JAVA_NODE} ! {self(), MsgId, get_next_auctions, Count},

  receive
    {MsgId, java_response, AuctionList} ->
      io:format("[MANAGER] Received ~p auctions from Java.~n", [length(AuctionList)]),
      AuctionList;

    {MsgId, java_error, Reason} ->
      io:format("[MANAGER] Java reported error: ~p~n", [Reason]),
      [];

    _Other -> []
  after 2000 ->
    io:format("[MANAGER] TIMEOUT: Java is not responding.~n"),
    []
  end.

%% Spawns both the Auction Handler and the Chat Handler for each item
start_auctions_batch([], State) ->
  State;

start_auctions_batch([AuctionData | Rest], State) ->
  {AuctionId, Name, Price} = AuctionData,
  io:format("[MANAGER] Spawning Auction and Chat: ID=~p, Item=~s~n", [AuctionId, Name]),

  %% 1. Spawn the auction logic process
  Pid = spawn(fun() -> 'DS_auction_handler':start(AuctionId, Price, Name, 30) end),

  %% 2. Spawn the chat process for this specific auction
  spawn(fun() -> chat_handler:start(AuctionId) end),

  %% 3. Monitor the logic process to free slots when it finishes
  'DS_auction_monitor':monitor_auction(Pid, AuctionId),

  %% 4. Update the Manager state map
  NewSlots = maps:put(Pid, AuctionId, State#state.active_slots),

  start_auctions_batch(Rest, State#state{active_slots = NewSlots}).