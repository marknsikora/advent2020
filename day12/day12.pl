#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(apply)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

input(Data) --> sequence(input_line, Data), eos.
input_line(X) --> direction(X), "\n".

direction(north(X)) --> "N", integer(X).
direction(south(X)) --> "S", integer(X).
direction(east(X)) --> "E", integer(X).
direction(west(X)) --> "W", integer(X).
direction(left(X)) --> "L", integer(X).
direction(right(X)) --> "R", integer(X).
direction(forward(X)) --> "F", integer(X).

load_data(Data, Name) :-
    open(Name, read, Stream),
    phrase_from_stream(input(Data), Stream).

move(north(N), position(X, Y, A), position(X, Y_, A)) :-
    Y_ is Y + N.

move(south(N), position(X, Y, A), position(X, Y_, A)) :-
    Y_ is Y - N.

move(east(N), position(X, Y, A), position(X_, Y, A)) :-
    X_ is X + N.

move(west(N), position(X, Y, A), position(X_, Y, A)) :-
    X_ is X - N.

move(left(N), position(X, Y, A), position(X, Y, A_)) :-
    A_ is A + N.

move(right(N), position(X, Y, A), position(X, Y, A_)) :-
    A_ is A - N.

move(forward(N), position(X, Y, A), position(X_, Y_, A)) :-
    X_ is X + round(cos(A * pi / 180)) * N,
    Y_ is Y + round(sin(A * pi / 180)) * N.

waypoint(north(N), position(SX, SY, WX, WY), position(SX, SY, WX, WY_)) :-
    WY_ is WY + N.

waypoint(south(N), position(SX, SY, WX, WY), position(SX, SY, WX, WY_)) :-
    WY_ is WY - N.

waypoint(east(N), position(SX, SY, WX, WY), position(SX, SY, WX_, WY)) :-
    WX_ is WX + N.

waypoint(west(N), position(SX, SY, WX, WY), position(SX, SY, WX_, WY)) :-
    WX_ is WX - N.

waypoint(left(N), position(SX, SY, WX, WY), position(SX, SY, WX_, WY_)) :-
    WX_ is WX * round(cos(N * pi / 180)) - WY * round(sin(N * pi / 180)),
    WY_ is WX * round(sin(N * pi / 180)) + WY * round(cos(N * pi / 180)).

waypoint(right(N), position(SX, SY, WX, WY), position(SX, SY, WX_, WY_)) :-
    WX_ is WX * round(cos(-N * pi / 180)) - WY * round(sin(-N * pi / 180)),
    WY_ is WX * round(sin(-N * pi / 180)) + WY * round(cos(-N * pi / 180)).

waypoint(forward(N), position(SX, SY, WX, WY), position(SX_, SY_, WX, WY)) :-
    SX_ is SX + WX * N,
    SY_ is SY + WY * N.

manhattan(X, Y, M) :- M is abs(X) + abs(Y).

example(1) :-
    load_data(Directions, 'sample'),
    foldl(move, Directions, position(0, 0, 0), position(X, Y, _)),
    manhattan(X, Y, M),
    M =:= 25.

example(2) :-
    load_data(Directions, 'sample'),
    foldl(waypoint, Directions, position(0, 0, 10, 1), position(X, Y, _, _)),
    manhattan(X, Y, M),
    M =:= 286.

star(1, M) :-
    load_data(Directions, 'input'),
    foldl(move, Directions, position(0, 0, 0), position(X, Y, _)),
    manhattan(X, Y, M).

star(2, M) :-
    load_data(Directions, 'input'),
    foldl(waypoint, Directions, position(0, 0, 10, 1), position(X, Y, _, _)),
    manhattan(X, Y, M).

main(_Argv) :-
    example(1),
    example(2),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
