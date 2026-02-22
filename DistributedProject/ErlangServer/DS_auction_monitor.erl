%%%-------------------------------------------------------------------
%%% @author crazy
%%% @doc
%%% DS_auction_monitor
%%% Manages the active monitoring of auction processes.
%%% Receives requests from the Manager to watch new auction handlers.
%%%-------------------------------------------------------------------
-module('DS_auction_monitor').
-behaviour(gen_server). %Erlang OTP behavior for implementing a generic server.

%% API
-export([start_link/0, monitor_auction/2]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%maps all the minotred auctions, MonitorRef -> {Pid, AuctionId}
-record(state, {
  monitored_auctions = #{} 
}).

%%Starts the monitor as part of the supervisor tree
%?MODULE to register the processw with the same name as the module
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%Function called by the Manager to register a new auction for monitoring
monitor_auction(Pid, AuctionId) ->
  gen_server:cast(?MODULE, {add_to_monitor, Pid, AuctionId}). %asynchronous cast


init([]) ->
  io:format("[MONITOR] Monitoring service started.~n"),
  {ok, #state{monitored_auctions = #{}}}.

%%Handles the request to start monitoring a specific PID
handle_cast({add_to_monitor, Pid, AuctionId}, State) ->
  %%create a monitor reference
  Ref = erlang:monitor(process, Pid),
  io:format("[MONITOR] Watching Auction ~p (Ref: ~p)~n", [AuctionId, Ref]),
  NewMap = maps:put(Ref, {Pid, AuctionId}, State#state.monitored_auctions),
  {noreply, State#state{monitored_auctions = NewMap}};

handle_cast(_Msg, State) ->
  {noreply, State}.

%catches the 'DOWN' message when a monitored process dies
handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
  case maps:find(Ref, State#state.monitored_auctions) of
    {ok, {Pid, AuctionId}} ->
      io:format("[MONITOR] Auction ~p (Pid ~p) exited. Reason: ~p~n", [AuctionId, Pid, Reason]),
      %forward the message to the manager
      case whereis('DS_auction_manager') of
        undefined -> ok;
        ManagerPid -> ManagerPid ! {'DOWN', Ref, process, Pid, Reason} %send the message
      end,

      NewMap = maps:remove(Ref, State#state.monitored_auctions),
      {noreply, State#state{monitored_auctions = NewMap}};
    error ->
      {noreply, State}
  end;

handle_info(_Info, State) ->
  {noreply, State}.

%unused callbacks
handle_call(_Request, _From, State) -> {reply, ok, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.