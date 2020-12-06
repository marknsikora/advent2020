#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(apply)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(yall)).

:- initialization(main, main).

input(Data) --> sequence(line, Data), eos.
line(X) --> sequence(nonblank, X), "\n", { member(_, X) }.

load_data(Data, Name) :-
    open(Name, read, Stream),
    phrase_from_stream(input(Data), Stream).

row(X, 1) :- char_code('B', X).
row(X, 0) :- char_code('F', X).

col(X, 1) :- char_code('R', X).
col(X, 0) :- char_code('L', X).

ticket(Seat, Row, Col) :-
    string_codes(Seat, Seat_),
    append(X, Y, Seat_),
    maplist(row, X, Row_),
    maplist(col, Y, Col_),
    foldl([A,B,C]>>(C is A + 2 * B), Row_, 0, Row),
    foldl([A,B,C]>>(C is A + 2 * B), Col_, 0, Col).

id(Seat, ID) :-
    ticket(Seat, Row, Col),
    ID is Row * 8 + Col.

example(1) :-
    ticket("BFFFBBFRRR", 70, 7).

example(2) :-
    ticket("FFFBBBFRRR", 14, 7).

example(3) :-
    ticket("BBFFBBFRLL", 102, 4).

star(1, X) :-
    load_data(Tickets, 'input'),
    maplist(id, Tickets, IDs),
    max_list(IDs, X).

star(2, X) :-
    load_data(Tickets, 'input'),
    maplist(id, Tickets, IDs),
    min_list(IDs, Min),
    max_list(IDs, Max),
    between(Min, Max, X),
    \+ member(X, IDs).

main(_Argv) :-
    example(1),
    example(2),
    example(3),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
