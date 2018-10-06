%%%-------------------------------------------------------------------
%%% Verteilte Systeme Praktikum - Aufgabe 1
%%% Sprache: Erlang/OTP, Datenstrukturen: Listen und Tupel
%%%
%%% @author Adrian Helberg
%%%-------------------------------------------------------------------
-module(app).
-author("Main").

%% API
-export([start/0]).

start() ->
  spawn(server, test, []),
  spawn(client, test, []),
  self().