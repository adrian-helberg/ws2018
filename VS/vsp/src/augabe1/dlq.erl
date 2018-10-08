%%%-------------------------------------------------------------------
%%% Module dlq, local ADT
%%% @author Adrian Helberg, Team 15
%%%-------------------------------------------------------------------
-module(dlq).
%% API
-export([start/0, stop/0]).

start() -> server:start(dlq).

stop() -> server:stop(dlq).