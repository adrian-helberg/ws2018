%%%-------------------------------------------------------------------
%%% @author Main
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Okt 2018 16:53
%%%-------------------------------------------------------------------
-module(vshlr).
-author("Main").

%% API
-export([start/0, stop/0, i_am_at/2, find/1, handle_event/2]).

start() -> server:start(xx1,
  fun(Event, State) ->
    handle_event(Event, State)
  end,
  []).

stop() -> server:stop(xx1).

i_am_at(Person, Position) -> server:call(xx1, {i_am_at, Person, Position}).
find(Person)              -> server:call(xx1, {find, Person}).


handle_event({i_am_at, Person, Position}, State) ->
  State1 = update_position(Person, Position, State),
  {ok, State1};
handle_event({find, Person}, State) ->
  Location = lookup(Person, State),
  {Location, State}.

update_position(Key, Value, [{Key, _}|T]) -> [{Key, Value}|T];
update_position(Key, Value, [H|T])        -> [H|update_position(Key, Value, T)];
update_position(Key, Value, [])           -> [{Key,Value}].

lookup(Key, [{Key, Value}|_]) -> {at, Value};
lookup(Key, [_|T])            -> lookup(Key, T);
lookup(Key, [])               -> lost.