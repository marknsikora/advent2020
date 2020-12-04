#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).

:- initialization(main, main).

input([X|Data]) -->
    integer(X),
    "\n",
    input(Data).

input([]) --> eos.

load_data(Data, Name) :-
    open(Name, read, Stream),
    phrase_from_stream(input(Data), Stream).

solve(Numbers, A, B) :-
    member(A, Numbers),
    member(B, Numbers),
    A + B =:= 2020.

solve(Numbers, A, B, C) :-
    member(A, Numbers),
    member(B, Numbers),
    member(C, Numbers),
    A + B + C =:= 2020.

example(1) :-
    load_data(Numbers, 'sample.input'),
    solve(Numbers, A, B),
    A * B =:= 514579.

example(2) :-
    load_data(Numbers, 'sample.input'),
    solve(Numbers, A, B, C),
    A * B * C =:= 241861950.

star(1, X) :-
    load_data(Numbers, 'input'),
    solve(Numbers, A, B),
    X is A * B.

star(2, X) :-
    load_data(Numbers, 'input'),
    solve(Numbers, A, B, C),
    X is A * B * C.

main(_Argv) :-
    example(1),
    example(2),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
