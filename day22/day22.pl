#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

input(PlayerOne, PlayerTwo) -->
    "Player 1:\n",
    sequence(integer, "\n", PlayerOne),
    "\n",
    "Player 2:\n",
    sequence(integer, "\n", PlayerTwo),
    eos.

combat([X|XS], [Y|YS], Z) :-
    X > Y,
    append(XS, [X, Y], Next),
    combat(Next, YS, Z).

combat([X|XS], [Y|YS], Z) :-
    X < Y,
    append(YS, [Y, X], Next),
    combat(XS, Next, Z).

combat([], X, X).
combat(X, [], X).

recursive_deck(X, XS, YS) :-
    append(YS, _, XS),
    proper_length(YS, X).

recursive_combat(X, Y, A, B) :- recursive_combat([], X, Y, A, B).

recursive_combat(_, [], X, [], X).
recursive_combat(_, X, [], X, []).

% Rule 1, seen before
recursive_combat(Previous, X, Y, X, []) :-
    memberchk([X,Y], Previous).

% Rule 3, recursive
recursive_combat(Previous, [X|XS], [Y|YS], A, B) :-
    recursive_deck(X, XS, XRec),
    recursive_deck(Y, YS, YRec),
    recursive_combat(XRec, YRec, XEnd, _YEnd),
    (   XEnd == []
    ->  Winner = 2
    ;   Winner = 1
    ),
    recursive_next_round(Previous, [X|XS], [Y|YS], Winner, A, B).

% Rule 4, normal round
recursive_combat(Previous, [X|XS], [Y|YS], A, B) :-
    (   X < Y
    ->  Winner = 2
    ;   Winner = 1
    ),
    recursive_next_round(Previous, [X|XS], [Y|YS], Winner, A, B).

recursive_next_round(Previous, [X|XS], [Y|YS], Winner, A, B) :-
    (   Winner =:= 1
    -> (append(XS, [X, Y], NextX), NextY = YS)
    ;  (append(YS, [Y, X], NextY), NextX = XS)
    ),
    !,
    recursive_combat([[[X|XS],[Y|YS]]|Previous], NextX, NextY, A, B).

score(XS, Z) :- score(XS, _, Z).

score([X|XS], N, Z) :-
    score(XS, N_, Z_),
    N #= N_ + 1,
    Z #= X * N + Z_.

score([], 0, 0).

example(1) :-
    phrase_from_file(input(PlayerOne, PlayerTwo), 'sample'),
    combat(PlayerOne, PlayerTwo, Winner),
    score(Winner, 306).

example(2) :-
    phrase_from_file(input(PlayerOne, PlayerTwo), 'sample'),
    recursive_combat(PlayerOne, PlayerTwo, OneEnd, TwoEnd),
    score(OneEnd, 0),
    score(TwoEnd, 291).

star(1, X) :-
    phrase_from_file(input(PlayerOne, PlayerTwo), 'input'),
    combat(PlayerOne, PlayerTwo, Winner),
    score(Winner, X).

star(2, X) :-
    phrase_from_file(input(PlayerOne, PlayerTwo), 'input'),
    recursive_combat(PlayerOne, PlayerTwo, OneEnd, TwoEnd),
    % Loosers score is always 0
    score(OneEnd, ScoreOne),
    score(TwoEnd, ScoreTwo),
    X #= ScoreOne + ScoreTwo.

main(_Argv) :-
    example(1),
    example(2),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
