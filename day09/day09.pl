#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

input(Data) --> sequence(input_line, Data), eos.
input_line(X) --> integer(X), "\n".

load_data(Data, Name) :-
    open(Name, read, Stream),
    phrase_from_stream(input(Data), Stream).

weakness(Preamble, [X|XS], Y) :-
    member(A, Preamble),
    member(B, Preamble),
    X =:= A + B,
    append(Preamble, [X], [_|Preamble_]),
    weakness(Preamble_, XS, Y).

weakness(_, [X|_], X).

crack(Numbers, N, X) :-
    append(_, Rest, Numbers),
    append(Range, _, Rest),
    proper_length(Range, L),
    L > 1,
    sum_list(Range, N),
    min_list(Range, Min),
    max_list(Range, Max),
    X is Min + Max.

example(1) :-
    load_data(Data, 'sample'),
    append(Preamble, Rest, Data),
    proper_length(Preamble, 5),
    weakness(Preamble, Rest, 127).

example(2) :-
    load_data(Data, 'sample'),
    crack(Data, 127, 62).

star(1, X) :-
    load_data(Data, 'input'),
    append(Preamble, Rest, Data),
    proper_length(Preamble, 25),
    weakness(Preamble, Rest, X).

star(2, X) :-
    load_data(Data, 'input'),
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
