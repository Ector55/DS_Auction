%%%-------------------------------------------------------------------
%%% @author crazy
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. gen 2026 17:05
%%%-------------------------------------------------------------------
-module('DS_auction_handler').
-author("crazy").

%% API
-export([start/4, loop/1]).
%%function for clock, for bids and winner declaration and the state of the aucitons
%%we used basic stuff for understanding

%%state of theauction
-record(state, {
  auction_pid,
  item_name,
  current_bid,
  high_bidder = none,
  time_remaining,
  bids_history = []
}).


start(AuctionPID, StartingPrice, ItemName, Time) ->
  io:format(" [AUCTION ~p] Started. Duration: ~p s~n", [AuctionPID, Time]),

  erlang:send_after(1000, self(), {clock}), %default built in timer for erlang, 1000 = 1s

  InitialState = #state{
    auction_pid = AuctionPID,
    item_name = ItemName,
    current_bid = StartingPrice,
    time_remaining = Time
  },
  loop(InitialState).

loop(State) ->
  end.


%handling of the bids
handle_bid(State, ClientPid, UserId, Amount) ->
  end.


%handling of the cloc
handle_clock(State) ->
  end.

%winner logic decision
handle_winner(State) ->
  end.