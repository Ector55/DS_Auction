%%%-------------------------------------------------------------------
%%% @author crazy
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%% it will start the auctions, it's the auction's parent,
%% it will generate the aucitons, need to add the crash handling
%%% @end
%%% Created : 02. gen 2026 17:04
%%%-------------------------------------------------------------------
-module('DS_auction_supervisor').
-author("crazy").
-behaviour(supervisor).

-export([start_link/0, start_auction/2, init/1]).

%% Starts the supervisor process decide if local or global
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%spawn delle auction dynamic, to add the specific parameters we want
%i used the ones from others
start_auction(AuctionID, EndTime, StartPrice, SellerID) ->
  supervisor:start_child(?MODULE, [AuctionID, EndTime, StartPrice, SellerID]).


%init([]) -> %to be defines i think this is the backup if auctions fail
