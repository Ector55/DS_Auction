%%%-------------------------------------------------------------------
%%% @author crazy
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. gen 2026 17:52
%%%-------------------------------------------------------------------
-module('DS_auction_monitor').
-author("crazy").

%% API
-export([start_monitor/0, monitor_auction/4]).

%% Funzione helper per interfacciare il monitor
monitor_auction(Pid, Name, Dur, Val) ->
  ?MODULE ! {monitor_new, Pid, Name, Dur, Val}.

start_monitor() ->
  register(?MODULE, self()),
  io:format("[MONITOR] Avviato.~n"),
  loop([]).

loop(MonitoredList) ->
  receive
  %% Aggiungi nuova asta da sorvegliare
    {monitor_new, Pid, Name, Dur, Val} ->
      monitor(process, Pid),
      loop([{Pid, Name, Dur, Val} | MonitoredList]);

  %% Il processo è morto normalmente (Asta finita) -> Rimuovi e basta
    {'DOWN', _Ref, process, Pid, normal} ->
      NewList = lists:keydelete(Pid, 1, MonitoredList),
      loop(NewList);

  %% Il processo è morto male (CRASH) -> RIAVVIA!
    {'DOWN', _Ref, process, Pid, Reason} ->
      io:format("[MONITOR] Asta ~p crashata per ~p. Respawning...~n", [Pid, Reason]),

      %% 1. Recupera i dati dalla lista locale
      case lists:keyfind(Pid, 1, MonitoredList) of
        {Pid, Name, Dur, Val} ->
          %% 2. Rilancia l'handler
          NewPid = spawn(fun() -> auction_handler:init(Name, Dur, Val) end),
          monitor(process, NewPid),

          %% 3. Aggiorna la lista del monitor
          NewList = lists:keydelete(Pid, 1, MonitoredList),

          %% 4. (Importante) Bisognerebbe avvisare il Main Server che il PID è cambiato
          %% Ma per semplicità qui omettiamo, l'asta continua a funzionare.

          loop([{NewPid, Name, Dur, Val} | NewList]);
        false ->
          loop(MonitoredList)
      end
  end.