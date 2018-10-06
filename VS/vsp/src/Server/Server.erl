%%%-------------------------------------------------------------------
%%% Server
%%% Verwaltung von Nachrichten, die von Redakteuren zugesendet werden
%%% Nachrichten sind eindeuting nummeriert (ID)
%%% Nachrichten werden in bestimmten Abständen von Lesern angefragt
%%% Server merkt sich geschickte Nachrichten von Lesern, die angefragt
%%% haben und vergisst sie nach einiger Zeit
%%%
%%% @author Adrian Helberg
%%%-------------------------------------------------------------------
-module(server).
-author("Main").

%% API
-export([server/1]).

server(State) ->
  receive {requiest, Return_PID} ->
    io:format("SERVER ~w: Client request received from ~w~n", [self(), Return_PID]),
    NewState = State + 1,
    Return_PID ! {hit_count, NewState},
    server(NewState)
  end.