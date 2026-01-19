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

-export([start_link/0 ]).

%%Starts the supervisor process decide if local or global
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%spawn delle auction dynamic, to add the specific parameters we want
%i used the ones from others
start_auction(AuctionID, EndTime, StartPrice, SellerID) ->
  supervisor:start_child(?MODULE, [AuctionID, EndTime, StartPrice, SellerID]).