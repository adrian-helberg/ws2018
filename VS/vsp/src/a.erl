% HBQ
-export([initHBQ/0, pushHBQ/3, deliverMSG/2, listDLQ/0, listHBQ/0, dellHBQ/0]).

initHBQ() -> {reply, ok}.
pushHBQ(NNr, Msg, TSclientout) -> {reply, ok}.
deliverMSG(NNr, ToClient) -> {reply, number}.
listDLQ() -> {reply, ok}.
listHBQ() -> {reply, ok}.
dellHBQ() -> {reply, ok}.

% DLQ
-export([initDLQ/2, delDLQ/1, expectedNr/1, push2DLQ/3, deliverMSG/4, listDLQ/1, lengthDLQ/1]).

initDLQ(Size,Datei) -> {reply, ok}.
delDLQ(Queue) -> {reply, ok}.
expectedNr(Queue) -> {reply, number}.
push2DLQ([NNr,Msg,TSclientout,TShbqin],Queue,Datei) -> {reply, ok}.
deliverMSG(MSGNr,ClientPID,Queue,Datei) -> {reply, {number, string, tuple, tuple}}.
listDLQ(Queue) -> {reply, ok}.
lengthDLQ(Queue) -> {reply, ok}.

% CMEM
-export([initCMEM/2, delCMEM/1, updateClient/4, getClientNNr/2, listCMEM/1, lengthCMEM/1]).

initCMEM(RemTime,Datei) -> {reply, ok}.
delCMEM(CMEM) -> {reply, ok}.
updateClient(CMEM,ClientID,NNr,Datei)  -> {reply, ok}.
getClientNNr(CMEM,ClientID) -> {reply, number}.
listCMEM(CMEM) -> {reply, [atom]}.
lengthCMEM(CMEM)  -> {reply, number}.