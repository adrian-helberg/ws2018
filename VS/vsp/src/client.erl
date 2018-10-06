%%%-------------------------------------------------------------------
%%% Client
%%% Redakteure und Leser (sequenzieller Wechsel)
%%%
%%% @author Adrian Helberg
%%%-------------------------------------------------------------------
-module(client).
-author("Main").

%% API
-export([client/1, test/0]).

client(Server_Address) ->
  Server_Address ! {request, self()},
  receive {hit_count, Number} ->
    io:format("Client ~w: Hit count was ~w~n", [self(), Number])
  end.

test() -> io:format("Client ~w: initialized ~n", [self()]).
