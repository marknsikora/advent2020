#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

input(Data) --> sequence(integer, ",", Data), "\n", eos.

load_data(Data, Name) :-
    open(Name, read, Stream),
    phrase_from_stream(input(Data), Stream).

number_game(XS, 0, XS).

number_game([X|XS], N, YS) :-
    nth1(X_, XS, X),
    M is N - 1,
    number_game([X_,X|XS], M, YS).

number_game(XS, N, YS) :-
    M is N - 1,
    number_game([0|XS], M, YS).

example(1) :-
    Start = [6, 3, 0],
    Rest = [0, 4, 0, 1, 3, 3, 0],
    append(Rest, Start, Final),
    proper_length(Rest, N),
    number_game(Start, N, Final).

example(2) :-
    Start = [2, 3, 1],
    X = 1,
    number_game(Start, 2017, [X|_]).

example(3) :-
    Start = [3, 1, 2],
    X = 10,
    number_game(Start, 2017, [X|_]).

example(4) :-
    Start = [3, 2, 1],
    X = 27,
    number_game(Start, 2017, [X|_]).

example(5) :-
    Start = [1, 3, 2],
    X = 78,
    number_game(Start, 2017, [X|_]).

example(6) :-
    Start = [1, 2, 3],
    X = 438,
    number_game(Start, 2017, [X|_]).

example(7) :-
    Start = [2, 1, 3],
    X = 1836,
    number_game(Start, 2017, [X|_]).

star(1, X) :-
    load_data(Numbers, 'input'),
    reverse(Numbers, Reverse),
    proper_length(Reverse, L),
    N is 2020 - L,
    number_game(Reverse, N, [X|_]).

main(_Argv) :-
    example(1),
    example(2),
    example(3),
    example(4),
    example(5),
    example(6),
    example(7),
    star(1, X),
    format('~d~n', [X]).
