%%%-------------------------------------------------------------------
%%% @author crazy
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%% this is to check wether the main server are wotking
%% if they crash or not this will be like the backup, for fault tolereance
%% there could be also the auction monitor that monitors the auctions
%%% @end
%%% Created : 02. gen 2026 17:04
%%%-------------------------------------------------------------------
-module('DS_auction_supervisor').
-author("crazy").
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  %% Avvia il supervisore locale registrato come my_supervisor
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_one, intensity => 3, period => 10},

  %% 1. Il Server Principale (GenServer)
  Server = #{id => main_server,
    start => {main_server, start_link, []},
    restart => permanent},

  %% 2. Il Monitor delle Aste (Processo semplice)
  Monitor = #{id => auctions_monitor,
    start => {auctions_monitor, start_monitor, []},
    restart => permanent},

  {ok, {SupFlags, [Server, Monitor]}}.