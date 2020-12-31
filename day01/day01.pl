#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

input(X) --> sequence(input_line, X), eos.
input_line(X) --> integer(X), "\n".

solve(Numbers, VS, X) :-
    sum(VS, #=, 2020),
    maplist({Numbers}/[A]>>member(A, Numbers), VS),
    foldl([A,B,C]>>(C #= A * B), VS, 1, X).

example(1) :-
    phrase_from_file(input(Numbers), 'sample'),
    solve(Numbers, [_,_], 514579).

example(2) :-
    phrase_from_file(input(Numbers), 'sample'),
    solve(Numbers, [_,_,_], 241861950).

star(1, X) :-
    phrase_from_file(input(Numbers), 'input'),
    solve(Numbers, [_,_], X).

star(2, X) :-
    phrase_from_file(input(Numbers), 'input'),
    solve(Numbers, [_,_,_], X).

main(_Argv) :-
    example(1),
    example(2),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
