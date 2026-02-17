-module(chat_handler).
-author("crazy").

%% API
-export([start/1, loop/2]).

-define(JAVA_NODE, 'java_node@10.2.1.25').
-define(JAVA_MAILBOX, 'java_listener').

%%Starts the chat process for a specific auction.
start(AuctionID) ->
  Name = list_to_atom("chat_" ++ integer_to_list(AuctionID)),

  case whereis(Name) of
    undefined ->
      register(Name, self()),
      io:format("[CHAT ~p] Started and registered as ~p~n", [AuctionID, Name]),
      loop(AuctionID, []);
    _Pid ->
      io:format("[CHAT ~p] Already exists, skipping start.~n", [AuctionID]),
      ok
  end.

loop(AuctionID, Participants) ->
  receive
  %%A user joins the auction chat from an Erlang terminal
    {join, ClientPid} ->
      io:format("[CHAT ~p] New participant: ~p~n", [AuctionID, ClientPid]),
      erlang:monitor(process, ClientPid),
      loop(AuctionID, [ClientPid | Participants]);

  %%user sends a message
    {post_message, FromUser, Content} ->
      %% show message on erlang console
      io:format("[CHAT ~p] ~s: ~s~n", [AuctionID, FromUser, Content]),
      Timestamp = erlang:system_time(second),

      %%BROADCAST to Erlang participants (Other terminals)
      Message = #{
        auction_id => AuctionID,
        user => FromUser,
        text => Content,
        time => Timestamp
      },
      lists:foreach(fun(Pid) -> Pid ! {chat_msg, Message} end, Participants),

      %%NOTIFY JAVA (To show messages on the Web Frontend)
      %%Protocol: {chat_msg, AuctionID, User, Content}
      {?JAVA_MAILBOX, ?JAVA_NODE} ! {chat_msg, AuctionID, FromUser, Content},

      loop(AuctionID, Participants);

    {'DOWN', _Ref, process, Pid, _Reason} ->
      loop(AuctionID, lists:delete(Pid, Participants));

    stop ->
      io:format("[CHAT ~p] Auction ended. Closing...~n", [AuctionID]),
      lists:foreach(fun(Pid) -> Pid ! {chat_closed, AuctionID} end, Participants),
      ok;

    _Other ->
      loop(AuctionID, Participants)
  end.