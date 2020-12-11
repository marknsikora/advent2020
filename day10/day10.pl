#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(apply)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(yall)).

:- initialization(main, main).

input(Data) --> sequence(input_line, Data), eos.
input_line(X) --> integer(X), "\n".

load_data(Data, Name) :-
    open(Name, read, Stream),
    phrase_from_stream(input(Data), Stream).

validate(1).
validate(2).
validate(3).

differences(Numbers, X, Y) :-
    sort(Numbers, Sorted),
    append(Start, [_], [0|Sorted]),
    maplist([A,B,C]>>(C is A - B), Sorted, Start, Diff),
    maplist(validate, Diff),
    occurrences_of_term(1, Diff, X),
    occurrences_of_term(3, Diff, Y_),
    Y is Y_ + 1.

arrangements(Numbers, X) :-
    sort([0|Numbers], Sorted),
    arrangements(Sorted, 0, 1, X).

arrangements([X,Y|Numbers], Streak, Comb, Z) :-
    X + 1 =:= Y,
    Streak_ is Streak + 1,
    arrangements([Y|Numbers], Streak_, Comb, Z).

arrangements([_|Numbers], 0, Comb, Z) :-
    arrangements(Numbers, 0, Comb, Z).

arrangements([_|Numbers], Streak, Comb, Z) :-
    Streak_ is Streak - 1,
    triangular(Streak_, T),
    Comb_ is Comb * (T + 1),
    arrangements(Numbers, 0, Comb_, Z).

arrangements([], _, X, X).

triangular(X, Y) :- Y is (X * (X + 1)) / 2.

example(1) :-
    load_data(Numbers, 'sample-1'),
    differences(Numbers, 7, 5).

example(2) :-
    load_data(Numbers, 'sample-2'),
    differences(Numbers, 22, 10).

example(3) :-
    load_data(Numbers, 'sample-1'),
    arrangements(Numbers, 8).

example(4) :-
    load_data(Numbers, 'sample-2'),
    arrangements(Numbers, 19208).

star(1, X) :-
    load_data(Numbers, 'input'),
    differences(Numbers, A, B),
    X is A * B.

star(2, X) :-
    load_data(Numbers, 'input'),
    arrangements(Numbers, X).

main(_Argv) :-
    example(1),
    example(2),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
