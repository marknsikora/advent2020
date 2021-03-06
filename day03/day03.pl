#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

input(X) --> sequence(input_line, X), eos.

input_line(X) --> sequence(terrain, X), "\n".

terrain(slope) --> ".".
terrain(tree) --> "#".

make_cycle(X, Cycle) :-
    make_cycle(X, Cycle, Cycle).

make_cycle([X|XS], cycle(X, Next), Final) :-
    make_cycle(XS, Next, Final).

make_cycle([X], cycle(X, Final), Final).

move_forward(N, cycle(_, Next), Y) :-
    N #> 0,
    M #= N - 1,
    move_forward(M, Next, Y).

move_forward(0, X, X).

move_down([], [], _).

move_down(X, X, 1).

move_down([_|XS], Y, N) :-
    M #= N - 1,
    move_down(XS, Y, M).

move(X, Y, Down, Right) :-
    move_down(X, T, Down),
    maplist(move_forward(Right), T, Y).

path([cycle(X, _)|XS], [X|YS], Down, Right) :-
    move(XS, Next, Down, Right),
    path(Next, YS, Down, Right).

path([], [], _, _).

solve(X, N, Down, Right) :-
    path(X, Path, Down, Right),
    occurrences_of_term(tree, Path, N).

example(1) :-
    phrase_from_file(input(Trees), 'sample'),
    maplist(make_cycle, Trees, Cycles),
    solve(Cycles, 7, 1, 3).

example(2) :-
    phrase_from_file(input(Trees), 'sample'),
    maplist(make_cycle, Trees, Cycles),
    solve(Cycles, A, 1, 1),
    solve(Cycles, B, 1, 3),
    solve(Cycles, C, 1, 5),
    solve(Cycles, D, 1, 7),
    solve(Cycles, E, 2, 1),
    A * B * C * D * E #= 336.

star(1, X) :-
    phrase_from_file(input(Trees), 'input'),
    maplist(make_cycle, Trees, Cycles),
    solve(Cycles, X, 1, 3).

star(2, X) :-
    phrase_from_file(input(Trees), 'input'),
    maplist(make_cycle, Trees, Cycles),
    solve(Cycles, A, 1, 1),
    solve(Cycles, B, 1, 3),
    solve(Cycles, C, 1, 5),
    solve(Cycles, D, 1, 7),
    solve(Cycles, E, 2, 1),
    X #= A * B * C * D * E.

main(_Argv) :-
    example(1),
    example(2),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
