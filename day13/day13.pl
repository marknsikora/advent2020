#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

input(Depart, Busses) -->
    depart_time(Depart),
    busses(Busses),
    eos.

depart_time(X) --> integer(X), "\n".

busses(X) --> sequence(bus_id, ",", X), "\n".

bus_id(X) --> integer(X).
bus_id(x) --> "x".

earlier_arrival(Time, X, Y, Z) :-
    NextX #= Time mod X,
    NextY #= Time mod Y,
    NextX #> NextY ->
        Z = X;
        Z = Y.

earliest_departure(Start, Busses, Z) :-
    include(number, Busses, Numbers),
    foldl(earlier_arrival(Start), Numbers, 1, X),
    Wait #= X - Start mod X,
    Z #= X * Wait.

enumerate_busses(Busses, X) :- enumerate_busses(Busses, 0, X).

enumerate_busses([x|Busses], N, X) :-
    M #= N + 1,
    enumerate_busses(Busses, M, X).

enumerate_busses([B|Busses], N, [B-N|X]) :-
    number(B),
    M #= N + 1,
    enumerate_busses(Busses, M, X).

enumerate_busses([], _, []).

prize(Busses, X) :-
    enumerate_busses(Busses, Enum),
    prize_(Enum, X).

prize_([X-N,Y-M|XS], A) :-
    % Find first step that these two numbers overlap
    X * I - N #= Y * _J - M,
    I in 1..Y,
    % Combine into one larger step for next iteration
    Z #= X * Y,
    O #= X * (Y - I) + N,
    prize_([Z-O|XS], A).

prize_([X-N], A) :- A #= X - N.

example(1) :-
    phrase_from_file(input(Depart, Busses), 'sample'),
    earliest_departure(Depart, Busses, 295).

example(2) :-
    phrase_from_file(input(_, Busses), 'sample'),
    prize(Busses, 1068781).

example(3) :- prize([17,x,13,19], 3417).
example(4) :- prize([67,7,59,61], 754018).
example(5) :- prize([67,x,7,59,61], 779210).
example(6) :- prize([67,7,x,59,61], 1261476).
example(7) :- prize([1789,37,47,1889], 1202161486).

star(1, X) :-
    phrase_from_file(input(Depart, Busses), 'input'),
    earliest_departure(Depart, Busses, X).

star(2, X) :-
    phrase_from_file(input(_, Busses), 'input'),
    prize(Busses, X).

main(_Argv) :-
    example(1),
    example(2),
    example(3),
    example(4),
    example(5),
    example(6),
    example(7),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
