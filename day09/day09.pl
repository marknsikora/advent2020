#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

input(Data) --> sequence(input_line, Data), eos.
input_line(X) --> integer(X), "\n".

weakness(Preamble, [X|XS], Y) :-
    member(A, Preamble),
    member(B, Preamble),
    X #= A + B,
    append(Preamble, [X], [_|Preamble_]),
    weakness(Preamble_, XS, Y).

weakness(_, [X|_], X).

crack(Numbers, N, X) :-
    append(_, Rest, Numbers),
    append(Range, _, Rest),
    proper_length(Range, L),
    L #> 1,
    sum_list(Range, N),
    min_list(Range, Min),
    max_list(Range, Max),
    X #= Min + Max.

example(1) :-
    phrase_from_file(input(Data), 'sample'),
    append(Preamble, Rest, Data),
    proper_length(Preamble, 5),
    weakness(Preamble, Rest, 127).

example(2) :-
    phrase_from_file(input(Data), 'sample'),
    crack(Data, 127, 62).

star(1, X) :-
    phrase_from_file(input(Data), 'input'),
    append(Preamble, Rest, Data),
    proper_length(Preamble, 25),
    weakness(Preamble, Rest, X).

star(2, X) :-
    phrase_from_file(input(Data), 'input'),
    append(Preamble, Rest, Data),
    proper_length(Preamble, 25),
    weakness(Preamble, Rest, N),
    crack(Data, N, X).

main(_Argv) :-
    example(1),
    example(2),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
