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
-export([start/4, loop/1]).

%% time synchronization api(Cristian's algortihm)
-export([get_server_time/1, get_server_time/2, sync_time/1, sync_time/2]).

-define(JAVA_NODE, 'java_node@127.0.0.1').
-define(JAVA_MAILBOX, 'java_listener').
%% State record
-record(state, {
  auction_id,
  item_name,
  current_bid,
  high_bidder = none,
  time_remaining,
  bids_history = [],
  extend_threshold = 30,  % Extend if bid arrives with <= 10 seconds left
  server_start_time       % time when auction started on server
}).


%% start an auction

start(AuctionID, StartingPrice, ItemName, Duration) ->
  io:format("[AUCTION ~p] Started. Item: '~s', Starting price: ~p, Duration: ~p s~n",
    [AuctionID, ItemName, StartingPrice, Duration]),

  erlang:send_after(1000, self(), clock),

  InitialState = #state{
    auction_id = AuctionID,
    item_name = ItemName,
    current_bid = StartingPrice,
    time_remaining = Duration,
    server_start_time = erlang:system_time(millisecond)
  },

%% Register the process locally so it can be reached by "auction_ID"
  register(list_to_atom("auction_" ++ integer_to_list(AuctionID)), self()),

  loop(InitialState).

%% main Loop - to receive and handle messages

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

          %% Log every 30 seconds can be removed later
          if
            NewTime rem 30 =:= 0 ->
              io:format("[AUCTION ~p] ~p seconds remaining~n",
                [State#state.auction_id, NewTime]);
            true ->
              ok
          end,

          loop(State#state{time_remaining = NewTime})
      end;

  %% BID - handle incoming bid

    {ClientPid, bid, UserId, Amount} ->
      NewState = handle_bid(State, ClientPid, UserId, Amount),
      loop(NewState);

  %% GET STATUS - return current auction state

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

  %% GET HISTORY - Return bid history

    {get_history, ClientPid} ->
      ClientPid ! {history, lists:reverse(State#state.bids_history)},
      loop(State);

  %% time synchronization using Cristian's algorithm
    {get_time, ClientPid} ->
      ServerTime = erlang:system_time(millisecond),
      ClientPid ! {time_response, ServerTime},
      loop(State);

  %% UNKNOWN MESSAGE - ignore and continue loop
    _Other ->
      loop(State)

  end.


%% Handle Bid - Validates and processes a bid

%% Handle Bid - Validates and processes a bid
handle_bid(State, ClientPid, UserId, Amount) ->
  AuctionID = State#state.auction_id,
  CurrentBid = State#state.current_bid,
  HighBidder = State#state.high_bidder,
  TimeRemaining = State#state.time_remaining,

  if
  %% Case 1: Auction ended
    TimeRemaining =< 0 ->
      io:format("[AUCTION ~p] Bid rejected: auction ended~n", [AuctionID]),
      %% Notify Java of the rejection
      {java_listener, 'java_node@127.0.0.1'} ! {bid_rejected, AuctionID, UserId, auction_ended},
      ClientPid ! {bid_rejected, auction_ended},
      State;

  %% Case 2: Bid too low
    Amount =< CurrentBid ->
      io:format("[AUCTION ~p] Bid rejected: ~p not higher than ~p~n",
        [AuctionID, Amount, CurrentBid]),
      %% Notify Java of the rejection
      {java_listener, 'java_node@127.0.0.1'} ! {bid_rejected, AuctionID, UserId, bid_too_low},
      ClientPid ! {bid_rejected, bid_too_low},
      State;

  %% Case 3: Consecutive bids from the same user
    UserId =:= HighBidder ->
      io:format("[AUCTION ~p] Bid rejected: user ~p cannot bid consecutively~n",
        [AuctionID, UserId]),
      %% Notify Java of the rejection
      {java_listener, 'java_node@127.0.0.1'} ! {bid_rejected, AuctionID, UserId, consecutive_bid},
      ClientPid ! {bid_rejected, consecutive_bid_not_allowed},
      State;
  %% Case 4: Bid ACCEPTED
    true ->
      io:format("[AUCTION ~p] Bid ACCEPTED: ~p by user ~p~n",
        [AuctionID, Amount, UserId]),

      %% Notify Java of the successful bid
      {java_listener, 'java_node@127.0.0.1'} ! {new_bid, AuctionID, Amount, UserId},
      %% Record bid in history
      Timestamp = erlang:system_time(second),
      BidRecord = #{
        user_id => UserId,
        amount => Amount,
        timestamp => Timestamp
      },
      NewHistory = [BidRecord | State#state.bids_history],

      %% Check if we need to extend time
      ExtendThreshold = State#state.extend_threshold,
      NewTime = if
                  TimeRemaining =< ExtendThreshold ->
                    io:format("[AUCTION ~p] Timer EXTENDED to 30 seconds~n", [AuctionID]),
                    {java_listener, 'java_node@127.0.0.1'} ! {bid_accepted, AuctionID, Amount, UserId},

                    30;
                  true ->
                    TimeRemaining
                end,

      %% Send success response to local Erlang bidder
      ClientPid ! {bid_accepted, Amount, NewTime},

      %% Return updated state
      State#state{
        current_bid = Amount,
        high_bidder = UserId,
        bids_history = NewHistory,
        time_remaining = NewTime
      }
  end.


%% handle Winner , when auction timer reaches 0

%% Handle Winner - called when the auction timer reaches 0
handle_winner(State) ->
  AuctionID = State#state.auction_id,
  Winner = State#state.high_bidder,
  FinalPrice = State#state.current_bid,
  ItemName = State#state.item_name,

  %% Format winner for Java compatibility
  JavaWinner = if Winner == none -> "no_winner"; true -> Winner end,

  %% 1. Identify and stop the associated chat process
  ChatName = list_to_atom("chat_" ++ integer_to_list(AuctionID)),
  case whereis(ChatName) of
    undefined -> ok;
    ChatPid -> ChatPid ! stop
  end,

  %% 2. Notify the Java listener about the auction results
  {java_listener, 'java_node@127.0.0.1'} ! {auction_closed, AuctionID, JavaWinner, FinalPrice},

  %% 3. Print auction summary to the console
  io:format("~n========================================~n"),
  case Winner of
    none ->
      io:format("[AUCTION ~p] ENDED - NO BIDS~n", [AuctionID]),
      io:format("Item '~s' received no bids.~n", [ItemName]);
    _ ->
      io:format("[AUCTION ~p] ENDED - SOLD!~n", [AuctionID]),
      io:format("Item: '~s'~n", [ItemName]),
      io:format("Winner: User ~p~n", [Winner]),
      io:format("Final Price: ~p~n", [FinalPrice]),
      io:format("Total Bids: ~p~n", [length(State#state.bids_history)])
  end,
  io:format("========================================~n~n"),

  %% 4. Notify the Manager that the auction logic is finished
  case whereis('DS_auction_manager') of
    undefined -> ok;
    ManagerPid -> ManagerPid ! {auction_ended, AuctionID, Winner, FinalPrice}
  end,

  %% 5. Unregister the process name to allow reuse of the ID later
  catch unregister(list_to_atom("auction_" ++ integer_to_list(AuctionID))),

  %% 6. TERMINATE the process
  %% This triggers the 'DOWN' message in DS_auction_manager,
  %% which immediately triggers the request for new auctions.
  io:format("[AUCTION ~p] Cleaning up process and freeing slot...~n", [AuctionID]),
  exit(normal).


%% CRISTIAN'S ALGORITHM - Time Synchronization

%% Algorithm:
%%   1. Client records T1 (local time before request)
%%   2. Client sends {get_time, self()} to auction process
%%   3. Server responds with {time_response, ServerTime}
%%   4. Client records T2 (local time after response)
%%   5. RTT = T2 - T1
%%   6. Offset = ServerTime + (RTT / 2) - T2

%% Get raw  server time from auction process
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

% synchronize local time with server time using Cristian's algorithm

% sync with local auction (same node)
sync_time(AuctionID) ->
  sync_time(AuctionID, node()).

% sync with remote auction (across nodes)
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