-module(hanoi).
-export([create_towers/1, display_towers/1, move/4]).

create_towers(N) ->
  [  {tower1,lists:seq(1,N)}, {tower2, []}, {tower3, []}  ].

display_towers(T) ->
%  S1 = lists:sort(element(2, lists:nth(1, T))),
%  S2 = lists:sort(element(2, lists:nth(2, T))),
%  S3 = lists:sort(element(2, lists:nth(3, T))),

  io:format("----------------------------- ~n"),
  io:format("~p: ~p ~n", [element(1, lists:nth(1, T)), lists:reverse(element(2, lists:nth(1, T)))]),
  io:format("~p: ~p ~n", [element(1, lists:nth(2, T)), lists:reverse(element(2, lists:nth(2, T)))]),
  io:format("~p: ~p ~n" , [element(1, lists:nth(3, T)), lists:reverse(element(2, lists:nth(3, T)))]),
  %io:format("~p: ~p ~n", [element(1, lists:nth(1, T)), lists:reverse(S1)]),
  %io:format("~p: ~p ~n", [element(1, lists:nth(2, T)), lists:reverse(S2)]),
  %io:format("~p: ~p ~n", [element(1, lists:nth(3, T)), lists:reverse(S3)]),
  io:format("----------------------------- ~n").


move(T, Source, Destination, Other) ->
  LengthSource= length(element(2,(lists:nth(Source,T)))),
  case LengthSource =:= 0 of
    true -> io:format("Illegal move detected. Can't take a disk from an empty tower! ~n");
    false->
      [Head|Tail] = element(2,(lists:nth(Source,T))),
      List1 = element(2,lists:nth(Destination,T)) ++ [Head],

      ListAdd = lists:sort(List1),
      ListRemove = lists:sort(Tail),

      SourceTuple = setelement(2, (lists:nth(Source, T)),ListRemove),
      DestTuple = setelement(2, (lists:nth(Destination, T)), ListAdd),
      OtherTuple = lists:nth(Other, T),

      NewList = [SourceTuple, DestTuple, OtherTuple],

      L1 = lists:keyfind(tower1, 1, NewList),
      L2 = lists:keyfind(tower2, 1, NewList),
      L3 = lists:keyfind(tower3, 1, NewList),
      FullList = [L1,L2,L3],

      display_towers(FullList),
      FullList
    end.



















%[{tower1,[List|create_towers(N-1)]}, {tower2, []}, {tower3, []}].
%[{tower1,[5|[4|[3|[2|[1]]]]]}, {tower2, []}, {tower3, []}].
%[N|create_towers(N-1)],
%[{tower1,}, {tower2, []}, {tower3, []}].

%Tails = [  {tower2, []}, {tower3, []} ],
%[Head|Tails].
%{tower1,[S|create_towers(S-1)]}.
%[N|create_towers(N-1)].
%[ {tower1, lists:S}, {tower2, []}, {tower3, []} ].
