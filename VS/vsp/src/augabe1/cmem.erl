%%%-------------------------------------------------------------------
%%% Module cmem, local ADT
%%% @author Adrian Helberg, Team 15
%%%-------------------------------------------------------------------
-module(cmem).
%% API
-export([start/0, stop/0]).

start() -> server:start(cmem).

stop() -> server:stop(cmem).