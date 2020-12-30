#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

input(X) --> sequence(input_line, X), eos.
input_line(X) --> integer(X), "\n".

solve(Numbers, A, B) :-
    member(A, Numbers),
    member(B, Numbers),
    A + B #= 2020.

solve(Numbers, A, B, C) :-
    member(A, Numbers),
    member(B, Numbers),
    member(C, Numbers),
    A + B + C #= 2020.

example(1) :-
    phrase_from_file(input(Numbers), 'sample'),
    solve(Numbers, A, B),
    A * B #= 514579.

example(2) :-
    phrase_from_file(input(Numbers), 'sample'),
    solve(Numbers, A, B, C),
    A * B * C #= 241861950.

star(1, X) :-
    phrase_from_file(input(Numbers), 'input'),
    solve(Numbers, A, B),
    X #= A * B.

star(2, X) :-
    phrase_from_file(input(Numbers), 'input'),
    solve(Numbers, A, B, C),
    X #= A * B * C.

main(_Argv) :-
    example(1),
    example(2),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
