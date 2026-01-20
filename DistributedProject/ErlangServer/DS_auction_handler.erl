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

-export([init/3]).
-include("macro.hrl").

init(Name, Duration, MinOffer) ->
  %% Avvia il timer interno
  erlang:send_after(1000, self(), tick),
  %% Stato iniziale: Nessun utente, nessuna offerta migliore
  loop(Name, Duration, MinOffer, [], _BestOffer={0, none}).

loop(Name, TimeLeft, MinOffer, Users, {BestVal, BestUser}) ->
  receive
  %% -- NUOVA OFFERTA --
    {ClientPid, new_offer, Bid, User} ->
      if
        Bid > BestVal ->
          %% Offerta valida
          NewBest = {Bid, User},

          %% 1. Aggiorna la UI Java (Realtime)
          ?JAVA_LISTENER ! {self(), update_ui_bid, Name, Bid, User, TimeLeft},

          %% 2. Rispondi al client Erlang (se serve)
          ClientPid ! {self(), {ok}},

          loop(Name, TimeLeft, MinOffer, Users, NewBest);
        true ->
          %% Offerta troppo bassa
          ClientPid ! {self(), {error, too_low}},
          loop(Name, TimeLeft, MinOffer, Users, {BestVal, BestUser})
      end;

  %% -- TIMER --
    tick ->
      NewTime = TimeLeft - 1,
      if
        NewTime =< 0 ->
          %% TEMPO SCADUTO
          winner(Name, BestUser);
        true ->
          %% Riavvia timer per il prossimo secondo
          erlang:send_after(1000, self(), tick),
          %% (Opzionale) Aggiorna UI Java col tempo
          ?JAVA_LISTENER ! {self(), update_ui_timer, Name, NewTime},
          loop(Name, NewTime, MinOffer, Users, {BestVal, BestUser})
      end;

  %% -- KILL SWITCH --
    kill_auction_suicide ->
      exit(killed);

    _ -> loop(Name, TimeLeft, MinOffer, Users, {BestVal, BestUser})
  end.

winner(Name, Winner) ->
  ActualWinner = case Winner of none -> "NoWinner"; _ -> Winner end,

  %% 1. Dì a Java di salvare il vincitore nel DB
  io:format("[HANDLER] Asta ~p finita. Vincitore: ~p. Avviso Java.~n", [Name, ActualWinner]),
  ?JAVA_LISTENER ! {self(), db_close_auction, Name, ActualWinner},

  %% 2. Avvisa il Main Server di rimuovermi dalla lista RAM
  gen_server:cast(main_server, {auction_ended, Name}),

  %% 3. Muori felice
  exit(normal).
