%%%-------------------------------------------------------------------
%%% @author crazy
%%% @doc
%%% DS_auction_supervisor
%%% Supervisor to supervise the manager and the monitor.
%% it spawns the monitor and than the manager
%% if the monitor crasges it will be restarted, to garantee availability
%% idem for the manager 
%%% @end
%%%-------------------------------------------------------------------
-module('DS_auction_supervisor').
-behaviour(supervisor).

%% API
-export([start_link/0, init/1]).

%%Starts the supervisor and registers it with the same name
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%sup callbacks
init([]) ->
  %%Supervision Flags:
  %% - strategy: one_for_one -> If a child dies, only that child is restarted.
  %% - intensity: 5 -> Max 5 restarts.
  %% - period: 10 -> within 10 seconds.
  SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},

  %%child 1 il monitor
  MonitorSpec = #{
    id => 'DS_auction_monitor',
    start => {'DS_auction_monitor', start_link, []},
    restart => permanent,
    type => worker
  },

  %%child 2 il manager
  ManagerSpec = #{
    id => 'DS_auction_manager',
    start => {'DS_auction_manager', start_link, []},
    restart => permanent,
    type => worker
  },

%%starting order
  ChildSpecs = [MonitorSpec, ManagerSpec],
  {ok, {SupFlags, ChildSpecs}}. %set flags