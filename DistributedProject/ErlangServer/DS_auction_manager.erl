%%%-------------------------------------------------------------------
%%% @author crazy
%%% @doc
%%% Auction Manager - handles the lifecycle of auction slots and
%%% coordinates between Java and Erlang handlers.
%%%-------------------------------------------------------------------
-module('DS_auction_manager').
-author("crazy").

-behaviour(gen_server). %erlang otp gen_server behavior

%% API
-export([start_link/0]).

%% GenServer Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%constants names to interact with java
-define(JAVA_NODE, 'java_node@10.2.1.25'). %java name
-define(WORKER_NODE, 'worker@10.2.1.13'). %worker node with handler and chat on different container
-define(JAVA_MAILBOX, 'java_listener'). %java mailbox for the messages 
-define(POLL_INTERVAL, 5000). %time interval for item requesting


-record(state, {
  active_slots = #{} :: map() %list for active auctions
}).

%%starts the manager server and registers it 
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%initialize the server and starts the periodic polling timer
init([]) ->
  io:format("[MANAGER] Server started. Waiting for Java...~n"),

  %%start the 3 permanent slots
  start_permanent_slots(),

  timer:send_interval(?POLL_INTERVAL, check_for_auctions),
  {ok, #state{active_slots = #{}}}.

handle_info(check_for_auctions, State) ->
  FreeSlots = get_free_slots(State#state.active_slots),

  case FreeSlots of
    [] ->
      io:format("[MANAGER] All slots busy. Skipping.~n"),
      {noreply, State};
    _ ->
      NumFree = length(FreeSlots),
      io:format("[MANAGER] ~p slots free. Requesting items from Java...~n", [NumFree]),

      NewItems = fetch_auctions_rpc(NumFree),
      NewState = load_items_into_slots(NewItems, FreeSlots, State),
      {noreply, NewState}
  end;

%%auction ended normally
handle_info({auction_ended, AuctionId, ItemId, Result}, State) ->
  io:format("[MANAGER] Auction ~p (Item ~p) notified end: ~p Waiting for process cleanup...~n", [AuctionId, ItemId, Result]),
  notify_java(Result, ItemId),

  NewSlots = maps:remove(AuctionId, State#state.active_slots),

  NewPid = spawn(?WORKER_NODE, 'DS_auction_handler', start, [AuctionId]),  timer:sleep(50),  %% wait for it to register
  timer:sleep(50),
  'DS_auction_monitor':monitor_auction(NewPid, AuctionId), %monitor the new auction/slot
  io:format("[MANAGER] Started new idle auction ~p (Pid: ~p)~n", [AuctionId, NewPid]),

  erlang:send_after(500, self(), check_for_items),

  {noreply, State#state{active_slots = NewSlots}};

%%monitor reports a slot crashed
handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
  io:format("[MANAGER] process ~p died: ~p~n", [Pid,Reason]),

  %%Only restart if the slot crashed (not on normal exit)
  case Reason of
    normal ->
      io:format("[MANAGER] Process ~p exited normally, ignoring~n", [Pid]),
      {noreply, State};
    _ ->
      %%find the slot that crashed
      CrashedSlot = find_slot_by_pid(Pid, State#state.active_slots),

      case CrashedSlot of
        undefined ->
          {noreply, State};
        AuctionId ->
          io:format("[MANAGER] Restarting auction ~p...~n", [AuctionId]),

          %% remove from active slot/auction list
          NewSlots = maps:remove(AuctionId, State#state.active_slots),
          %%restart the slot
          NewPid = spawn(?WORKER_NODE, 'DS_auction_handler', start, [AuctionId]),
          timer:sleep(50),
          'DS_auction_monitor':monitor_auction(NewPid,AuctionId),

          {noreply, State#state{active_slots = NewSlots}}
      end
  end;

%get the active auctions  
handle_info({FromPid, Ref, get_active_auctions}, State) ->
  ActiveList = maps:fold(fun(AuctionId, {Pid, ItemId}, Acc) ->

    TimeLeft = try
      gen_server:call(Pid, get_remaining_time, 100) 
    catch
      _:_ -> 0 %% If the call fails consider time left as 0, meaning the auction is ending or has ended
    end,

    [{AuctionId, ItemId, TimeLeft} | Acc] % accumulate the list of active auctions
        end, [], State#state.active_slots),

  FromPid ! {Ref, active_auctions_response, ActiveList}, %reply to the caller
  {noreply, State};

handle_info(Info, State) ->
  io:format("[MANAGER] Unexpected message: ~p~n", [Info]),
  {noreply, State}.

%unused callbacks
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%start the 3 permanent auction slot processes
start_permanent_slots() ->
  lists:foreach(fun(AuctionId) ->
    AuctionName = list_to_atom("auction_" ++ integer_to_list(AuctionId)),
    case rpc:call(?WORKER_NODE, erlang, whereis, [AuctionName]) of %remote call to check if the auction process already exists
      undefined ->
        Pid = spawn(?WORKER_NODE, 'DS_auction_handler', start, [AuctionId]), %spawns the auction handler for the slots

        'DS_auction_monitor':monitor_auction(Pid, AuctionId),
        io:format("[MANAGER] Auction ~p SPAWNED on REMOTE WORKER ~p (Pid: ~p)~n", [AuctionId, ?WORKER_NODE, Pid]);

      ExistingPid ->
        io:format("[MANAGER] Auction ~p already running on ~p (Pid: ~p)~n", [AuctionId, ?WORKER_NODE, ExistingPid])
    end
                end, [1,2,3]).


%%get list of slots that don't have an active item
get_free_slots(ActiveSlots) ->
  AllSlots = [1, 2, 3],
  BusySlots = maps:keys(ActiveSlots),
  AllSlots -- BusySlots.

%load the items received from java into the free slots, and update the state with the new active auctions
load_items_into_slots([], _FreeSlots, State) ->
  State;
load_items_into_slots(_Items, [], State) ->
  State;
%take the first item and the first free slot, load it and repeat recursively for the rest 3
load_items_into_slots([{ItemId, Name, Price} | RestItems], [AuctionId | RestSlots], State) -> 
  AuctionName = list_to_atom("auction_" ++ integer_to_list(AuctionId)),
  Duration = 300, %%duration of the auctions
%try to find the auction process on the worker node, if it doesn't exist log an error and skip, otherwise load the item into it
  case rpc:call(?WORKER_NODE, erlang, whereis, [AuctionName]) of
    undefined ->
      io:format("[MANAGER] Error: Auction slot ~p not found on worker!~n", [AuctionId]),
      load_items_into_slots(RestItems,RestSlots, State);
    Pid ->
      io:format("[MANAGER] Loading Item#~p '~s' into auction ~p~n", [ItemId, Name, AuctionId]),
      Pid ! {load_item, ItemId, Name, Price, Duration}, %sends the item details to the auction handler
      spawn(?WORKER_NODE, chat_handler, start, [AuctionId]), %%start the chat handler for this auction
      NewSlots = maps:put(AuctionId,{Pid,ItemId}, State#state.active_slots),
      load_items_into_slots(RestItems, RestSlots, State#state{active_slots = NewSlots})
  end.

%%find which slot a PID belongs to
find_slot_by_pid(Pid, ActiveSlots) ->
  Result = maps:filter(fun(_AuctionId, {P, _ItemId}) -> P =:= Pid end, ActiveSlots),
  case maps:keys(Result) of
    [AuctionId | _] -> AuctionId;
    [] -> undefined
  end.

%java notifications for different auction results 
notify_java({sold , Winner, Price}, ItemId) ->
  io:format("[MANAGER] Notifying Java: Item ~p SOLD to ~p for ~p~n", [ItemId, Winner, Price]),
  {?JAVA_MAILBOX, ?JAVA_NODE} ! {auction_closed, ItemId, Winner, Price};

notify_java(no_bids, ItemId) ->
  io:format("[MANAGER] Notifying Java: Item ~p UNSOLD (back to pending)~n", [ItemId]),
  {?JAVA_MAILBOX, ?JAVA_NODE} !{auction_unsold, ItemId}.

%%fetch Pending auctions
fetch_auctions_rpc(Count) ->
  MsgId = make_ref(),

  {?JAVA_MAILBOX, ?JAVA_NODE} ! {self(), MsgId, get_next_auctions, Count}, %send the request to java with a unique reference
  %waiting for different responses
  receive
    {MsgId, java_response, AuctionList} ->
      io:format("[MANAGER] Received ~p auctions from Java.~n", [length(AuctionList)]),
      AuctionList;

    {MsgId, java_error, Reason} ->
      io:format("[MANAGER] Java reported error: ~p~n", [Reason]),
      [];
  %timeour if java doesn't respond in time
    _Other -> []
  after 5000 ->
    io:format("[MANAGER] TIMEOUT: Java is not responding.~n"),
    []
  end.
