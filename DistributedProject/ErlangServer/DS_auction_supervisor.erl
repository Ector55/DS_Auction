-module('DS_auction_supervisor').
-behaviour(supervisor). %supervisor for fault tolerance purposes
%this observes the monitor and restarts it if anything goes wrong
%standardOTP for a supervisor

%% API Exports
-export([start_link/0, init/1]).

%fatto simile all'altro
%%starts the supervisor and registers it locally.
start_link() ->
%%register the supervisor locally as 'DS_auction_supervisor', idk if local or global
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).
%starts the supervisor and starts the vchildren in init

%%if the Monitor dies, only the Monitor is restarted (not the whole system).
init([]) ->
  %Monitor crasha più di 1 volta in 5 secondi, tutto il supervisor muore.
  %% Supervision Flags:
  %% - strategy: one_for_one -> if one child dies, just that will be restarted
  %% - intensity: 1 (Max 1 restart...)
  %% - period: 5 (...every 5 seconds)
  %% This prevents infinite restart loops if there is a critical persistent bug.
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},

  %child list
  %define who needs to be observed
  ChildSpecs = [
    %CHILD 1: The Auction Monitor (Manager)
    %this process is CRITICAL: it manages the 3 slots and talks to Java.
    %if it crashes, the Supervisor will restart it immediately (permanent).
    #{
      id => 'DS_auction_monitor',
      start => {'DS_auction_monitor', start_link, []},
      restart => permanent,
      shutdown => 2000,   % Wait 2s for graceful shutdown
      type => worker,
      modules => ['DS_auction_monitor']
    }
  ],

  {ok, {SupFlags, ChildSpecs}}.