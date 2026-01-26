-module(chat_handler).
-author("crazy").

%% API
-export([start/1, loop/2]).

%% Avvia il processo di chat per un'asta specifica
start(AuctionID) ->
  Name = list_to_atom("chat_" ++ integer_to_list(AuctionID)),
  register(Name, self()),
  loop(AuctionID, []).

loop(AuctionID, Participants) ->
  receive
  %% Un utente si unisce alla chat dell'asta
    {join, ClientPid} ->
      io:format("[CHAT ~p] Nuovo partecipante: ~p~n", [AuctionID, ClientPid]),
      loop(AuctionID, [ClientPid | Participants]);

  %% Un utente invia un messaggio
    {post_message, FromUser, Content} ->
      Timestamp = erlang:system_time(second),
      Message = #{
        auction_id => AuctionID,
        user => FromUser,
        text => Content,
        time => Timestamp
      },
      %% Invia il messaggio a tutti i partecipanti (broadcast)
      lists:foreach(fun(Pid) -> Pid ! {chat_msg, Message} end, Participants),
      loop(AuctionID, Participants);

  %% Gestione chiusura chat al termine dell'asta
    stop ->
      io:format("[CHAT ~p] Chiusura in corso...~n", [AuctionID]),
      ok;

    _Other ->
      loop(AuctionID, Participants)
  end.