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
-export([start/3, stop/1, loop/3, call/2]).

start(Name, F, State) ->
  register(Name, spawn(server, loop, [Name, F, State])).

stop(Name) ->
  exit(whereis(Name), kill).

call(Name, Query) ->
  Name ! {self(), Query},
  receive
    {Name, Reply} ->
      Reply
  end.

loop(Name, F, State) ->
  receive
    {Pid, Query} ->
      {Reply, State1} = F(Query, State),
      Pid ! {Name, Reply},
      loop(Name, F, State1)
  end.