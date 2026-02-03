%%%-------------------------------------------------------------------
%%% @author crazy
%%% @doc
%%% DS_auction_monitor
%%% Manages the active monitoring of auction processes.
%%% Receives requests from the Manager to watch new auction handlers.
%%%-------------------------------------------------------------------
-module('DS_auction_monitor').
-behaviour(gen_server).

%% API
-export([start_link/0, monitor_auction/2]).

%% GenServer Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  monitored_auctions = #{} %% Map: MonitorRef -> {Pid, AuctionId}
}).

%% ===================================================================
%% API
%% ===================================================================

%% Starts the monitor as part of the supervisor tree
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Function called by the Manager to register a new auction for monitoring
monitor_auction(Pid, AuctionId) ->
  gen_server:cast(?MODULE, {add_to_monitor, Pid, AuctionId}).

%% ===================================================================
%% GenServer Callbacks
%% ===================================================================

init([]) ->
  io:format("[MONITOR] Monitoring service started.~n"),
  {ok, #state{monitored_auctions = #{}}}.

%% Handles the request to start monitoring a specific PID
handle_cast({add_to_monitor, Pid, AuctionId}, State) ->
  %% Create a monitor reference
  Ref = erlang:monitor(process, Pid),
  io:format("[MONITOR] Watching Auction ~p (Ref: ~p)~n", [AuctionId, Ref]),
  NewMap = maps:put(Ref, {Pid, AuctionId}, State#state.monitored_auctions),
  {noreply, State#state{monitored_auctions = NewMap}};

handle_cast(_Msg, State) ->
  {noreply, State}.

%% Automatically called when a monitored process exits
%% Handles the signal 'DOWN' when a monitored process ends or crashes
handle_info({'DOWN', Ref, process, _Pid, Reason}, State) ->
  %% Use _TargetPid (or just _) instead of _Pid to avoid the shadowing warning
  case maps:find(Ref, State#state.monitored_auctions) of
    {ok, {_TargetPid, AuctionId}} ->
      case Reason of
        normal ->
          io:format("[MONITOR] Auction ~p finished normally.~n", [AuctionId]);
        _Other ->
          io:format("[MONITOR] ALERT: Auction ~p crashed! Reason: ~p~n", [AuctionId, Reason])
      end,
      NewMap = maps:remove(Ref, State#state.monitored_auctions),
      {noreply, State#state{monitored_auctions = NewMap}};
    error ->
      {noreply, State}
  end;

handle_info(_Info, State) ->
  {noreply, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.