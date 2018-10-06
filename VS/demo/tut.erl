-module(tut).
-compile(export_all).

sum(N) -> sum(N,0).	 
sum([],N) -> N;
sum([H|T],Accu) -> sum(T,H+Accu).

sumD([]) -> 0;
sumD([H|T]) -> H + sumD(T).

convert(M,inch) -> M / 2.54;
convert(M,centimeter) -> M * 2.54;	 
convert(_M,A) -> io:format("Fehler ~p\n",[A]).	 

convert_length({centimeter,Val}) -> {inch,convert(Val,inch)};
convert_length({inch,Val}) -> {centimeter,convert(Val,centimeter)}.

convert_lengthD(Length) ->
    case Length of
        {centimeter, X} ->
            {inch, X / 2.54};
        {inch, Y} ->
            {centimeter, Y * 2.54}
    end.

test_if(A, B) ->
    if 
        A == 5 ->
            io:format("A == ~p~n", [A]),
            a_equals_5;
        B == 6 ->
            io:format("B == 6~n", []),
            b_equals_6;
        A == 2, B == 3 ->                      %That is A equals 2 and B equals 3
            io:format("A == 2, B == 3~n", []),
            a_equals_2_b_equals_3;
        A == 1 ; B == 7 ->                     %That is A equals 1 or B equals 7
            io:format("A == 1 ; B == 7~n", []),
            a_equals_1_or_b_equals_7
    end.

month_length(Year, Month) ->
    %% All years divisible by 400 are leap
    %% Years divisible by 100 are not leap (except the 400 rule above)
    %% Years divisible by 4 are leap (except the 100 rule above)
    Leap = if
        trunc(Year / 400) * 400 == Year ->
            leap;
        trunc(Year / 100) * 100 == Year ->
            not_leap;
        trunc(Year / 4) * 4 == Year ->
            leap;
        true ->
            not_leap
    end,  
    case Month of
        sep -> 30;
        apr -> 30;
        jun -> 30;
        nov -> 30;
        feb when Leap == leap -> 29;
        feb -> 28;
        jan -> 31;
        mar -> 31;
        may -> 31;
        jul -> 31;
        aug -> 31;
        oct -> 31;
        dec -> 31
    end.

%% Löscht das letzte Element einer Liste
%  Beispielaufruf: Erg = delete_last([a,b,c])
%
delete_last(List) -> delete_last(List,[]).
%
delete_last([_H|[]],NewList) -> NewList;
  delete_last([H|T],NewList) -> delete_last(T,NewList++[H]).
  
delete_lastB([_H|[]]) -> [];
delete_lastB([H|T]) -> [H|delete_lastB(T)].  

zeitsum(ZahlN) -> zeitsumR(13,lists:seq(1, ZahlN),0,0).
zeitsumR(Count,SumListe,Time,TimeD) when Count > 0 -> 
			io:format("."),
			Start = erlang:timestamp(),
			sum(SumListe),
			Ende = erlang:timestamp(),
			io:format("."),
			StartD = erlang:timestamp(),
			sumD(SumListe),
			EndeD = erlang:timestamp(),
			Diffms = float_to_int(timer:now_diff(Ende,Start)/1000),
			DiffmsD = float_to_int(timer:now_diff(EndeD,StartD)/1000),
			zeitsumR(Count-1,SumListe,Time+Diffms,TimeD+DiffmsD);
zeitsumR(_Count,_SumListe,Time,TimeD) -> io:format("\n"),{float_to_int(Time/13),float_to_int(TimeD/13)}.	

float_to_int(Float) -> list_to_integer(float_to_list(Float, [{decimals, 0}])).

powerset(Liste) -> selectionS(generate(Liste)).
generate([]) -> [[]];
generate([H|T]) -> PT = generate(T),
  generate(H, PT, PT).

generate(_, [], Acc) -> Acc;
generate(X, [H|T], Acc) -> generate(X, T, [[X|H]|Acc]).

selectionS([]) -> [];
selectionS(Liste) -> {Min,RListe} = getmin(Liste),
                     [Min|selectionS(RListe)].

% findet das Minimum der Liste
% kann nicht gelöscht werden, weil sonst Zahlen vertauscht werden
getmin([]) -> {-1,[]};
getmin([Head|Tail]) -> 
		{RMin,RListe} = getmin(Head,Tail),
                case RMin of
					Head -> {RMin,RListe}; % loesche Min
					_Other -> {RMin,[Head|RListe]}
				end.

getmin(Min,[]) -> {Min,[]};
getmin(Min,[Head|Tail]) when length(Min) < length(Head) -> 
				{RMin,RListe} = getmin(Min,Tail),
                {RMin,[Head|RListe]};			 
getmin(_Min,[Head|Tail]) -> 
				{RMin,RListe} = getmin(Head,Tail),
                case RMin of
					Head -> {RMin,RListe}; % loesche Min
					_Other -> {RMin,[Head|RListe]}
				end.
