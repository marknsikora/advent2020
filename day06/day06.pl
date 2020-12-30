#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(yall)).

:- initialization(main, main).

input(Data) --> sequence(group, "\n", Data), eos.
group(X) --> sequence(line, X).
line(X) --> sequence(nonblank, X), "\n", { member(_, X) }.

anyone -->
    maplist([A, B]>>foldl(union, A, [], B)),
    maplist(proper_length),
    sum_list.

everyone -->
    maplist([[A|AS], B]>>foldl(intersection, AS, A, B)),
    maplist(proper_length),
    sum_list.

example(1) :-
    phrase_from_file(input(Answers), 'sample'),
    anyone(Answers, 11).

example(2) :-
    phrase_from_file(input(Answers), 'sample'),
    everyone(Answers, 6).

star(1, X) :-
    phrase_from_file(input(Answers), 'input'),
    anyone(Answers, X).

star(2, X) :-
    phrase_from_file(input(Answers), 'input'),
    everyone(Answers, X).

main(_Argv) :-
    example(1),
    example(2),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
