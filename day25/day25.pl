#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).

:- initialization(main, main).

input(X, Y) --> integer(X), "\n", integer(Y), "\n".

find_key(S, G, X) :- find_key(S, G, 0, 1, X).

find_key(_, V, X, V, X).
find_key(S, G, N, V, X) :-
    N #>= 0,
    N_ #= N + 1,
    V_ #= (V * S) mod 20201227,
    find_key(S, G, N_, V_, X).

example(1) :-
    phrase_from_file(input(CardPublic, DoorPublic), 'sample'),
    CardLoops = 8,
    find_key(7, CardPublic, CardLoops),
    DoorLoops = 11,
    find_key(7, DoorPublic, DoorLoops),
    EncryptionKey = 14897079,
    find_key(DoorPublic, EncryptionKey, CardLoops),
    find_key(CardPublic, EncryptionKey, DoorLoops).

star(1, X) :-
    phrase_from_file(input(CardPublic, DoorPublic), 'input'),
    find_key(7, CardPublic, CardLoops),
    find_key(DoorPublic, X, CardLoops).

main(_Argv) :-
    example(1),
    star(1, X),
    format('~d~n', [X]).
