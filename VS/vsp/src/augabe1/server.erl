%%%-------------------------------------------------------------------
%%% Module server
%%% @author Adrian Helberg, Team 15
%%%-------------------------------------------------------------------
-module(server).
%% API
-export([start/1, stop/1, call/2, log/2]).

%%% Starts the server
% Name  - Name of the server
start(Name) ->
  % Register process to be given name so that this name can be used as identifier
  % register(RegName, PidOrPort) -> true
  % Associates the name RegName with a process identifier
  % RegName:atom can be used instead of the pid in send operator (RegName ! Message)
  register(
    Name,
    % Create process
    % spawn(Node, Fun) -> pid()
    % Returns the PID of new process started
    spawn(fun() -> loop() end)
  ),
  io:format("~p ~w server started~n", [vsutil:now2string(erlang:timestamp()), Name]).

%%% Stops the server
% Name  - Name of the server
stop(Name) ->
  % Sends an exit signal with exit reason Reason to the process
  % exit(Pid, Reason) -> true
  exit(
    % Returns the process identifier with the registered name
    whereis(Name),
    kill
  ),
  io:format("~p ~w server stopped~n", [vsutil:now2string(erlang:timestamp()), Name]).

%%% Makes a synchronous call to process by sending a request
% Name  - Name of the process
% Query - Request to process
call(Name, Query) ->
  % Send own PID and the request to process
  Name ! {self(), Query},
  receive
    {} ->
      io:format("Server call received something")
  end.

%%% Server loop
loop() ->
  receive
    {PID, getmessages} ->
      {reply,
        [ 15,
          "0-pclient@KI-VS-<0.64.0>-KLC: 15te_Nachricht. C Out: 29.04 08:43:24,871| HBQ In: 29.04 08:43:24,879|  DLQ Out: 29.04 08:43:35,551| ",
          {1430,289804,872001},
          {1430,289804,880000},
          {1430,289804,880003},
          {1430,289815,551001}],
        true},
        loop();
      _ ->
        io:format("~p server received something~n", [vsutil:now2string(erlang:timestamp())]),
        loop()
  end.

log(Datei,Text) ->
  file:write_file(Datei,Text,[append]),
  io:format("~p~n", Text).