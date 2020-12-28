#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

input(X) --> sequence(input_line, X), eos, !.
input_line(X) --> sequence(cube, X), "\n".

cube(active) --> "#".
cube(inactive) --> ".".

active_cube(Grid, [X, Y|_]) :-
    nth0(Y, Grid, Row),
    nth0(X, Row, active).

point_neighbour(Point0, Point) :-
    same_length(Point0, VS),
    VS ins -1..1,
    maplist([A,B,C]>>(C #= A + B), Point0, VS, Point),
    label(VS).

point_neighbours(Point, Z) :- findall(A, point_neighbour(Point, A), Z).

run(Points, N, Z) :-
    pairs_keys_values(Pairs, Points, Points),
    list_to_assoc(Pairs, Start),
    run_(Start, N, End),
    assoc_to_keys(End, Z).

run_(Active, 0, Active).
run_(Active, N0, Z) :-
    N0 #> 0,
    N #= N0 - 1,
    step(Active, NextActive),
    run_(NextActive, N, Z).

step(Active, NextActive) :-
    findall(X, (gen_assoc(T, Active, _), point_neighbour(T, X)), CheckPoints),
    sort(CheckPoints, Sorted),
    include(rule(Active), Sorted, Next),
    pairs_keys_values(Pairs, Next, Next),
    list_to_assoc(Pairs, NextActive),
    !.

rule(Active, Point) :-
    point_neighbours(Point, Neighbours),
    include({Active}/[A]>>get_assoc(A, Active, _), Neighbours, ActiveNeighbours),
    proper_length(ActiveNeighbours, L),
    (   get_assoc(Point, Active, _)
    ->  ( L #= 3 ; L #= 4 )
    ;   L #= 3
    ).

example(1) :-
    phrase_from_file(input(Grid), 'sample'),
    findall(A, (active_cube(Grid, A), A=[_,_,0]), Active),
    run(Active, 6, Z),
    proper_length(Z, 112).

example(2) :-
    phrase_from_file(input(Grid), 'sample'),
    findall(A, (active_cube(Grid, A), A=[_,_,0,0]), Active),
    run(Active, 6, Z),
    proper_length(Z, 848).

star(1, X) :-
    phrase_from_file(input(Grid), 'input'),
    findall(A, (active_cube(Grid, A), A=[_,_,0]), Active),
    run(Active, 6, Z),
    proper_length(Z, X).

star(2, X) :-
    phrase_from_file(input(Grid), 'input'),
    findall(A, (active_cube(Grid, A), A=[_,_,0,0]), Active),
    run(Active, 6, Z),
    proper_length(Z, X).

main(_Argv) :-
    example(1),
    example(2),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
