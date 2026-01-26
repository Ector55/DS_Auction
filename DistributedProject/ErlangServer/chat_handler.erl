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
  %% user joinf auction chat
    {join, ClientPid} ->
      io:format("[CHAT ~p] New partecipant: ~p~n", [AuctionID, ClientPid]),
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
      %broadcast message to partecipants
      lists:foreach(fun(Pid) -> Pid ! {chat_msg, Message} end, Participants),
      loop(AuctionID, Participants);

  %% shutting down after the auction
    stop ->
      io:format("[CHAT ~p] Closing...~n", [AuctionID]),
      ok;

    _Other ->
      loop(AuctionID, Participants)
  end.