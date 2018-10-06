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
-module('Server').
-author("Main").

%% API
-export([]).
