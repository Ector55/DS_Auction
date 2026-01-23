% %%%-------------------------------------------------------------------
% %%% @author crazy
% %%% @copyright (C) 2026, <COMPANY>
% %%% @doc
% %%%
% %%% @end
% %%% Created : 02. gen 2026 17:05
% %%%-------------------------------------------------------------------
% -module('DS_auction_handler').
% -author("crazy").

% -export([init/3]).
% -include("macro.hrl").
% %made by gemini need to work on this

% init(Name, Duration, MinOffer) ->
%   %% Avvia il timer interno
%   erlang:send_after(1000, self(), tick),
%   %% Stato iniziale: Nessun utente, nessuna offerta migliore
%   loop(Name, Duration, MinOffer, [], _BestOffer={0, none}).

% loop(Name, TimeLeft, MinOffer, Users, {BestVal, BestUser}) ->
%   receive
%   %% -- NUOVA OFFERTA --
%     {ClientPid, new_offer, Bid, User} ->
%       if
%         Bid > BestVal ->
%           %% Offerta valida
%           NewBest = {Bid, User},

%           %% 1. Aggiorna la UI Java (Realtime)
%           ?JAVA_LISTENER ! {self(), update_ui_bid, Name, Bid, User, TimeLeft},

%           %% 2. Rispondi al client Erlang (se serve)
%           ClientPid ! {self(), {ok}},

%           loop(Name, TimeLeft, MinOffer, Users, NewBest);
%         true ->
%           %% Offerta troppo bassa
%           ClientPid ! {self(), {error, too_low}},
%           loop(Name, TimeLeft, MinOffer, Users, {BestVal, BestUser})
%       end;

%   %% -- TIMER --
%     tick ->
%       NewTime = TimeLeft - 1,
%       if
%         NewTime =< 0 ->
%           %% TEMPO SCADUTO
%           winner(Name, BestUser);
%         true ->
%           %% Riavvia timer per il prossimo secondo
%           erlang:send_after(1000, self(), tick),
%           %% (Opzionale) Aggiorna UI Java col tempo
%           ?JAVA_LISTENER ! {self(), update_ui_timer, Name, NewTime},
%           loop(Name, NewTime, MinOffer, Users, {BestVal, BestUser})
%       end;

%   %% -- KILL SWITCH --
%     kill_auction_suicide ->
%       exit(killed);

%     _ -> loop(Name, TimeLeft, MinOffer, Users, {BestVal, BestUser})
%   end.

% winner(Name, Winner) ->
%   ActualWinner = case Winner of none -> "NoWinner"; _ -> Winner end,

%   %% 1. Dì a Java di salvare il vincitore nel DB
%   io:format("[HANDLER] Asta ~p finita. Vincitore: ~p. Avviso Java.~n", [Name, ActualWinner]),
%   ?JAVA_LISTENER ! {self(), db_close_auction, Name, ActualWinner},

%   %% 2. Avvisa il Main Server di rimuovermi dalla lista RAM
%   gen_server:cast(main_server, {auction_ended, Name}),

%   %% 3. Muori felice
%   exit(normal).
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

%% State record
-record(state, {
  auction_id,
  item_name,
  current_bid,
  high_bidder = none,
  time_remaining,
  bids_history = [],
  extend_threshold = 10  % Extend if bid arrives with <= 10 seconds left
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
    time_remaining = Duration
  },
  
  %% register this process so it can be found by auction ID
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
          handle_winner(State); %% Auction ended go to handle winner
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

    %% UNKNOWN MESSAGE - ignore and continue loop
     _Other ->
      loop(State)
   
  end.


%% Handle Bid - Validates and processes a bid

handle_bid(State, ClientPid, UserId, Amount) ->
  AuctionID = State#state.auction_id,
  CurrentBid = State#state.current_bid,
  HighBidder = State#state.high_bidder,
  TimeRemaining = State#state.time_remaining,
  

  if
    %% Check if auction is still ongoing
    TimeRemaining =< 0 ->
      io:format("[AUCTION ~p] Bid rejected: auction ended~n", [AuctionID]),
      ClientPid ! {bid_rejected, auction_ended},
      State;
    
    %% Check if bid is higher than current price
    Amount =< CurrentBid ->
      io:format("[AUCTION ~p] Bid rejected: ~p not higher than ~p~n", 
                [AuctionID, Amount, CurrentBid]),
      ClientPid ! {bid_rejected, bid_too_low},
      State;
    
    %% Check so that there are no consecutive bids)
    UserId =:= HighBidder ->
      io:format("[AUCTION ~p] Bid rejected: user ~p cannot bid consecutively~n", 
                [AuctionID, UserId]),
      ClientPid ! {bid_rejected, consecutive_bid_not_allowed},
      State;
    
    %% if all checks passed - accept the bid
    true ->
      io:format("[AUCTION ~p] Bid ACCEPTED: ~p by user ~p~n", 
                [AuctionID, Amount, UserId]),
      
      %% Record bid in history
      Timestamp = erlang:system_time(second),
      BidRecord = #{
        user_id => UserId, 
        amount => Amount, 
        timestamp => Timestamp
      },
      NewHistory = [BidRecord | State#state.bids_history],
      
      %% Check if we need to extend time (bid in last 10 seconds)
      ExtendThreshold = State#state.extend_threshold,
      NewTime = if
        TimeRemaining =< ExtendThreshold ->
          io:format("[AUCTION ~p] Timer EXTENDED to 30 seconds~n", [AuctionID]),
          30;
        true ->
          TimeRemaining
      end,
      
      %% Send success response to bidder
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

handle_winner(State) ->
  AuctionID = State#state.auction_id,
  Winner = State#state.high_bidder,
  FinalPrice = State#state.current_bid,
  ItemName = State#state.item_name,
  
  case Winner of
    none ->
      io:format("~n========================================~n"),
      io:format("[AUCTION ~p] ENDED - NO BIDS~n", [AuctionID]),
      io:format("Item '~s' received no bids.~n", [ItemName]),
      io:format("========================================~n~n");
    _ ->
      io:format("~n========================================~n"),
      io:format("[AUCTION ~p] ENDED - SOLD!~n", [AuctionID]),
      io:format("Item: '~s'~n", [ItemName]),
      io:format("Winner: User ~p~n", [Winner]),
      io:format("Final Price: ~p~n", [FinalPrice]),
      io:format("Total Bids: ~p~n", [length(State#state.bids_history)]),
      io:format("========================================~n~n")
  end,
  
  %% Notify manager that auction ended 
  case whereis('DS_auction_manager') of
    undefined -> 
      ok;
    ManagerPid -> 
      ManagerPid ! {auction_ended, AuctionID, Winner, FinalPrice}
  end,
  
  %% Unregister this process
  catch unregister(list_to_atom("auction_" ++ integer_to_list(AuctionID))),
  
  %% Return final result (process terminates)
  {ended, #{
    auction_id => AuctionID,
    winner => Winner,
    final_price => FinalPrice,
    item_name => ItemName,
    total_bids => length(State#state.bids_history)
  }}.