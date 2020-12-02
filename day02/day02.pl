#!/usr/bin/env swipl

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).

:- initialization(main, main).

input([password(Min, Max, Char, Password)|Data]) -->
    integer(Min),
    "-",
    integer(Max),
    blanks,
    nonblank(Char),
    ":",
    blanks,
    nonblanks(Password),
    "\n",
    input(Data).

input([]) --> eos.

load_data(Data, Name) :-
    open(Name, read, Stream),
    phrase_from_stream(input(Data), Stream).

validate(password(Min, Max, Char, Password)) :-
    occurrences_of_term(Char, Password, Count),
    Count =< Max,
    Count >= Min.

revalidate(password(Min, Max, Char, Password)) :-
    nth1(Min, Password, A),
    nth1(Max, Password, B),
    A \= B,
    ( A =:= Char ; B =:= Char ).

solve(Passwords, F, X) :-
    include(F, Passwords, Valid),
    proper_length(Valid, X).

example(1) :-
    load_data(Passwords, 'sample'),
    solve(Passwords, validate, X),
    X =:= 2.

example(2) :-
    load_data(Passwords, 'sample'),
    solve(Passwords, revalidate, X),
    X =:= 1.

star(1, X) :-
    load_data(Passwords, 'input'),
    solve(Passwords, validate, X).

star(2, X) :-
    load_data(Passwords, 'input'),
    solve(Passwords, revalidate, X).

main(_Argv) :-
    example(1),
    example(2),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
