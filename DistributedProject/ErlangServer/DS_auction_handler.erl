%%%-------------------------------------------------------------------
%%% @author crazy
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%% Auction Handler - manages timer, bids, and winner selection
%%% Each auction runs as a separate process
%%% @end
%%% Created : 02. gen 2026 17:05
%%%-------------------------------------------------------------------
-module('DS_auction_handler').
-author("crazy").

%% API
-export([start/1, loop/1]).

-export([get_server_time/1, get_server_time/2, sync_time/1, sync_time/2]).

-define(JAVA_NODE, 'java_node@10.2.1.25').
-define(MANAGER_NODE, 'auction_service@10.2.1.48').
-define(JAVA_MAILBOX, 'java_listener').

%%State record
-record(state, {
  auction_id,
  item_id,
  item_name,
  current_bid,
  high_bidder = none,
  high_bidder_name = "None",
  time_remaining,
  bids_history = [],
  extend_threshold = 30,
  server_start_time
}).


%%permanent slot: starts in idle mode, waits for items to be assigned
start(AuctionID) ->
  register(list_to_atom("auction_" ++ integer_to_list(AuctionID)), self()),
  idle_loop(AuctionID).

%%waits for an item to load
idle_loop(AuctionID) ->
  receive
    {load_item, ItemId, ItemName, StartingPrice, Duration} ->
      io:format("[AUCTION ~p] Loaded new item: ~s (Id = ~p) Starting price: ~p, Duration: ~p s~n",
        [AuctionID, ItemName, ItemId, StartingPrice, Duration]),
      erlang:send_after(1000, self(), clock),
      InitialState = #state{
        auction_id = AuctionID,
        item_id = ItemId,
        item_name = ItemName,
        current_bid = StartingPrice,
        time_remaining = Duration,
        server_start_time = erlang:system_time(millisecond)
      },
      loop(InitialState);
      {ClientPid, bid, _UserId, _Amount, UserName} ->
      io:format("[AUCTION ~p] Bid rejected for ~s: auction not started yet~n", [AuctionID, UserName]),
      {?JAVA_MAILBOX, ?JAVA_NODE} ! {bid_rejected, AuctionID, UserName, not_started},
      ClientPid ! {bid_rejected, auction_not_started},
      idle_loop(AuctionID);

    {get_status, ClientPid} ->
      ClientPid ! {status, #{status => not_started}},
      idle_loop(AuctionID);

    _Other ->
      io:format("[AUCTION ~p] Ignored unexpected message in idle_loop: ~p~n", [AuctionID, _Other]),
      idle_loop(AuctionID)
  end.


%%main Loop - to receive and handle messages

loop(State) ->
  receive
  %% CLOCK TICK - Decrements timer every second
    clock ->
      NewTime = State#state.time_remaining - 1,

      if
        NewTime =< 0 ->
          handle_winner(State); %% Auction ended go to handle_winner
        true ->
          erlang:send_after(1000, self(), clock),  %% Continue countdown
          if
            NewTime rem 30 =:= 0 ->
              io:format("[AUCTION ~p] ~p seconds remaining~n",
                [State#state.auction_id, NewTime]);
            true ->
              ok
          end,

          loop(State#state{time_remaining = NewTime})
      end;

  %%handle incoming bid

    {ClientPid, bid, UserId, Amount, UserName} ->
      NewState = handle_bid(State, ClientPid, UserId, Amount, UserName),
      loop(NewState);

  %%return current auction state

    {get_status, ClientPid} ->
      Status = #{
        auction_id => State#state.auction_id,
        item_name => State#state.item_name,
        current_bid => State#state.current_bid,
        high_bidder => State#state.high_bidder,
        time_remaining => State#state.time_remaining,
        status => ongoing
      },
      ClientPid ! {status, Status},
      loop(State);

  %%Return bid history

    {get_history, ClientPid} ->
      ClientPid ! {history, lists:reverse(State#state.bids_history)},
      loop(State);

    {get_time, ClientPid} ->
      ServerTime = erlang:system_time(millisecond),
      ClientPid ! {time_response, ServerTime},
      loop(State);

    {'$gen_call', {From, MRef}, get_remaining_time} ->
      From ! {MRef, State#state.time_remaining},
      loop(State);

  %%UNKNOWN MESSAGE - ignore and continue loop
    _Other ->
      loop(State)

  end.

%%Validates and processes a bid
handle_bid(State, ClientPid, UserId, Amount, UserName) ->
  AuctionID = State#state.auction_id,
  CurrentBid = State#state.current_bid,
  HighBidder = State#state.high_bidder,
  TimeRemaining = State#state.time_remaining,

  if
  %%Case 1: Auction ended
    TimeRemaining =< 0 ->
      io:format("[AUCTION ~p] Bid rejected for ~s: auction ended~n", [AuctionID, UserName]),
      {?JAVA_MAILBOX, ?JAVA_NODE} ! {bid_rejected, AuctionID, UserName, auction_ended},
      ClientPid ! {bid_rejected, auction_ended},
      State;

  %%Case 2: Bid too low
    Amount =< CurrentBid ->
      io:format("[AUCTION ~p] Bid rejected for ~s: ~p not higher than ~p~n", [AuctionID, UserName, Amount, CurrentBid]),
      {?JAVA_MAILBOX, ?JAVA_NODE} ! {bid_rejected, AuctionID, UserName, bid_too_low},
      ClientPid ! {bid_rejected, bid_too_low},
      State;

  %%Case 3: Consecutive bids from the same user
    UserId =:= HighBidder ->
      io:format("[AUCTION ~p] Bid rejected: ~s cannot bid consecutively~n", [AuctionID, UserName]),
      {?JAVA_MAILBOX, ?JAVA_NODE} ! {bid_rejected, AuctionID, UserName, consecutive_bid},
      ClientPid ! {bid_rejected, consecutive_bid_not_allowed},
      State;

  %%Case 4: Bid ACCEPTED
    true ->
      io:format("[AUCTION ~p] Bid ACCEPTED: ~p by user ~s~n", [AuctionID, Amount, UserName]),
      {?JAVA_MAILBOX, ?JAVA_NODE} ! {new_bid, AuctionID, Amount, UserName},

      %% Record bid in history with the name
      Timestamp = erlang:system_time(second),
      BidRecord = #{
        user_id => UserId,
        user_name => UserName,
        amount => Amount,
        timestamp => Timestamp
      },
      NewHistory = [BidRecord | State#state.bids_history],

      %%Check if we need to extend time
      ExtendThreshold = State#state.extend_threshold,
      NewTime = if
                  TimeRemaining =< ExtendThreshold ->
                    io:format("[AUCTION ~p] Timer EXTENDED for ~s~n", [AuctionID, UserName]),
                    {?JAVA_MAILBOX, ?JAVA_NODE} ! {bid_accepted, AuctionID, Amount, UserName},
                    30;
                  true ->
                    TimeRemaining
                end,
      ClientPid ! {bid_accepted, Amount, NewTime}, %success message

      State#state{
        current_bid = Amount,
        high_bidder = UserId,
        high_bidder_name = UserName,
        bids_history = NewHistory,
        time_remaining = NewTime
      }
  end.

%%Handle Winner - called when the auction timer reaches 0
handle_winner(State) ->
  AuctionID = State#state.auction_id,
  Winner = State#state.high_bidder,
  FinalPrice = State#state.current_bid,
  ItemName = State#state.item_name,

  ChatName = list_to_atom("chat_" ++ integer_to_list(AuctionID)),
  case whereis(ChatName) of
    undefined -> ok;
    ChatPid -> ChatPid ! stop
  end,

  case Winner of
    none ->
      %% no bids - items will go back to pending
      {?JAVA_MAILBOX, ?JAVA_NODE} ! {auction_unsold, State#state.item_id},
      io:format("[AUCTION ~p] No bids - item ~p will be returned to pending state~n", [AuctionID, State#state.item_id]);
    _ ->
      {JavaWinner,WinnerName} = case Winner of
                                  Winner when is_list(Winner) -> {list_to_binary(Winner), State#state.high_bidder_name};
                                  Winner when is_atom(Winner) -> {list_to_binary(atom_to_list(Winner)), State#state.high_bidder_name};
                                  Winner -> {list_to_binary(io_lib:format("~p", [Winner])), State#state.high_bidder_name}
                                end,
      {?JAVA_MAILBOX, ?JAVA_NODE} ! {auction_closed, State#state.item_id, JavaWinner, WinnerName, FinalPrice},
      io:format("[AUCTION ~p] SOLD! ~s for ~p to ~s~n",
        [AuctionID, ItemName, FinalPrice, State#state.high_bidder_name])
      end,

  %%Notify the Manager that the auction logic is finished
  { 'DS_auction_manager', ?MANAGER_NODE } ! {auction_ended, AuctionID, State#state.item_id,
    case Winner of
      none -> no_bids;
      _ -> {sold, Winner, FinalPrice}
    end
  },

  %%Unregister the process name to allow reuse of the ID later
  catch unregister(list_to_atom("auction_" ++ integer_to_list(AuctionID))),

  %% TERMINATE the process

  io:format("[AUCTION ~p] Cleaning up process and freeing slot...~n", [AuctionID]),
  exit(normal).


%%Get raw  server time from auction process
get_server_time(AuctionID) ->
  get_server_time(AuctionID, node()).

get_server_time(AuctionID, Node) ->
  AuctionName = list_to_atom("auction_" ++ integer_to_list(AuctionID)),

  {AuctionName, Node} ! {get_time, self()},
  receive
    {time_response, ServerTime} ->
      {ok, ServerTime}
  after 5000 ->
    {error, timeout}
  end.

%synchronize local time with server time using Cristian's algorithm
% sync with local auction (same node)
sync_time(AuctionID) ->
  sync_time(AuctionID, node()).

%sync with remote auction (across nodes)
sync_time(AuctionID, Node) ->
  AuctionName = list_to_atom("auction_" ++ integer_to_list(AuctionID)),
  T1 = erlang:system_time(millisecond),
  {AuctionName, Node} ! {get_time, self()},
  receive
    {time_response, ServerTime} ->
      T2 = erlang:system_time(millisecond),
      RTT = T2 - T1,
      Offset = ServerTime + (RTT div 2) - T2,
      io:format("[TIME SYNC] Auction ~p@~p: RTT=~p ms, Offset=~p ms~n",
        [AuctionID, Node, RTT, Offset]),
      {ok, #{offset => Offset, rtt => RTT, server_time => ServerTime, node => Node}}
  after 5000 ->
    {error, timeout}
  end.