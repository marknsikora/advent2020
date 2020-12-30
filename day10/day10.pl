#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(yall)).

:- initialization(main, main).

input(Data) --> sequence(input_line, Data), eos.
input_line(X) --> integer(X), "\n".

validate(1).
validate(2).
validate(3).

differences(Numbers, X, Y) :-
    sort(Numbers, Sorted),
    append(Start, [_], [0|Sorted]),
    maplist([A,B,C]>>(C #= A - B), Sorted, Start, Diff),
    maplist(validate, Diff),
    occurrences_of_term(1, Diff, X),
    occurrences_of_term(3, Diff, Y_),
    Y #= Y_ + 1.

arrangements(Numbers, X) :-
    sort([0|Numbers], Sorted),
    arrangements(Sorted, 0, 1, X).

arrangements([X,Y|Numbers], Streak, Comb, Z) :-
    Y #= X + 1,
    Streak_ #= Streak + 1,
    arrangements([Y|Numbers], Streak_, Comb, Z).

arrangements([_|Numbers], 0, Comb, Z) :-
    arrangements(Numbers, 0, Comb, Z).

arrangements([_|Numbers], Streak, Comb, Z) :-
    Streak_ #= Streak - 1,
    triangular(Streak_, T),
    Comb_ #= Comb * (T + 1),
    arrangements(Numbers, 0, Comb_, Z).

arrangements([], _, X, X).

triangular(X, Y) :- Y #= (X * (X + 1)) // 2.

example(1) :-
    phrase_from_file(input(Numbers), 'sample-1'),
    differences(Numbers, 7, 5).

example(2) :-
    phrase_from_file(input(Numbers), 'sample-2'),
    differences(Numbers, 22, 10).

example(3) :-
    phrase_from_file(input(Numbers), 'sample-1'),
    arrangements(Numbers, 8).

example(4) :-
    phrase_from_file(input(Numbers), 'sample-2'),
    arrangements(Numbers, 19208).

star(1, X) :-
    phrase_from_file(input(Numbers), 'input'),
    differences(Numbers, A, B),
    X #= A * B.

star(2, X) :-
    phrase_from_file(input(Numbers), 'input'),
    arrangements(Numbers, X).

main(_Argv) :-
    example(1),
    example(2),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
