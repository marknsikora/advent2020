#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

input(Data) --> sequence(integer, ",", Data), "\n", eos.

number_game(Start, Ages) :-
    prepare_ages(Start, State),
    lazy_list(number_game_, State, Ages).

prepare_ages(X, Y) :-
    empty_assoc(Ages),
    prepare_ages(X, Ages, 1, Y).

prepare_ages([X|XS], Ages, N, Y) :-
    put_assoc(X, Ages, N, NextAges),
    M is N + 1,
    prepare_ages(XS, NextAges, M, Y).

prepare_ages([X], Ages, N, state(Ages, X, N)).

number_game_(state(Ages, Last, N), state(NextAges, Next, M), Next) :-
    get_assoc(Last, Ages, X),
    Next is N - X,
    put_assoc(Last, Ages, N, NextAges),
    M is N + 1,
    !.

number_game_(state(Ages, Last, N), state(NextAges, 0, M), 0) :-
    M is N + 1,
    put_assoc(Last, Ages, N, NextAges),
    !.

example(1) :-
    Start = [0, 3, 6],
    Next = [0, 3, 3, 1, 0, 4, 0],
    number_game(Start, Final),
    append(Next, _, Final).

example(2) :-
    Start = [1, 3, 2],
    X = 1,
    number_game(Start, Rest),
    nth1(2017, Rest, X).

example(3) :-
    Start = [2, 1, 3],
    X = 10,
    number_game(Start, Rest),
    nth1(2017, Rest, X).

example(4) :-
    Start = [1, 2, 3],
    X = 27,
    number_game(Start, Rest),
    nth1(2017, Rest, X).

example(5) :-
    Start = [2, 3, 1],
    X = 78,
    number_game(Start, Rest),
    nth1(2017, Rest, X).

example(6) :-
    Start = [3, 2, 1],
    X = 438,
    number_game(Start, Rest),
    nth1(2017, Rest, X).

example(7) :-
    Start = [3, 1, 2],
    X = 1836,
    number_game(Start, Rest),
    nth1(2017, Rest, X).

example(8) :-
    Start = [0, 3, 6],
    X = 175594,
    number_game(Start, Rest),
    nth1(29999997, Rest, X).

star(1, X) :-
    phrase_from_file(input(Numbers), 'input'),
    proper_length(Numbers, L),
    number_game(Numbers, Rest),
    N is 2020 - L,
    nth1(N, Rest, X).

star(2, X) :-
    phrase_from_file(input(Numbers), 'input'),
    proper_length(Numbers, L),
    number_game(Numbers, Rest),
    N is 30000000 - L,
    nth1(N, Rest, X).

main(_Argv) :-
    example(1),
    example(2),
    example(3),
    example(4),
    example(5),
    example(6),
    example(7),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
