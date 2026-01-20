%%%-------------------------------------------------------------------
%%% @author crazy
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%% main server, Maintains a Map of AuctionID -> PID
%%% @end
%%% Created : 02. gen 2026 17:10
%%%-------------------------------------------------------------------
-module('DS_auction_manager').
-author("crazy").

-behaviour(gen_server).
-include("macro.hrl").
%made by gemini need to work on this

%% API
-export([start_link/0, login/2, create_auction/5, get_active_auctions/0]).
%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% --- API ---
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

login(User, Pass) ->
  gen_server:call(?MODULE, {login, User, Pass}).

create_auction(Name, Dur, Val, Img, Creator) ->
  gen_server:call(?MODULE, {new_auction, Name, Dur, Val, Img, Creator}).

get_active_auctions() ->
  gen_server:call(?MODULE, get_active_auctions).

%% --- CALLBACKS ---

init([]) ->
  %% All'avvio, lo stato è vuoto.
  %% (Opzionale: qui potresti chiedere a Java "dammi le aste attive" per ripristinarle in caso di crash)
  {ok, _ActiveAuctions = []}.


handle_call({login, User, Pass}, _From, State) ->
  %% NON controllo io. Chiedo a Java se l'utente è valido.
  Result = ask_java({check_login, User, Pass}),
  {reply, Result, State};


handle_call({new_auction, Name, Dur, Val, Img, Creator}, _From, ActiveAuctions) ->
  %% 1. Spawna il processo Erlang (RAM)
  Pid = spawn(fun() -> auction_handler:init(Name, Dur, Val) end),

  %% 2. Avvisa Java di salvare l'asta nel DB
  %% Nota: invio asincrono (!) perché non voglio bloccare il server se Java è lento a scrivere su DB
  ?JAVA_LISTENER ! {self(), db_insert_auction, Name, Dur, Val, Img, Creator},

  %% 3. Avvisa il Monitor (così se crasha lo riavvia) %questo magari da cambiare pk è identico alle auctions
  auctions_monitor:monitor_auction(Pid, Name, Dur, Val),

  %% 4. Aggiorna stato locale
  NewAuctions = [{Name, Pid, Dur, Val, Creator} | ActiveAuctions],

  {reply, {ok, Pid}, NewAuctions};


handle_call(get_active_auctions, _From, ActiveAuctions) ->
  {reply, {ok, ActiveAuctions}, ActiveAuctions}.


%% Gestisce la fine di un'asta (messaggio che arriva dall'handler)
handle_cast({auction_ended, Name}, ActiveAuctions) ->
  io:format("[MAIN SERVER] Rimuovo asta conclusa: ~p~n", [Name]),
  NewList = lists:keydelete(Name, 1, ActiveAuctions),
  {noreply, NewList};

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.


%% --- INTERNAL HELPER ---
%% Questa funzione serve per aspettare una risposta da Java (simula una chiamata sincrona)
ask_java(Message) ->
  Ref = make_ref(), % Crea un ID univoco per la richiesta
  ?JAVA_LISTENER ! {self(), Ref, Message}, % Invia a Java: "Ecco il mio ID, rispondimi con questo ID"
  receive
    {Ref, Reply} -> Reply % Aspetta la risposta con lo stesso ID
  after 2000 ->
    {error, java_timeout} % Se Java non risponde in 2 secondi
  end.