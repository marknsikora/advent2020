#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(yall)).

:- initialization(main, main).

input(Rules, Yours, Nearby) -->
    sequence(rule_line, Rules),
    "\n",
    "your ticket:\n",
    ticket_line(Yours),
    "\n",
    "nearby tickets:\n",
    sequence(ticket_line, Nearby),
    eos, !.

rule_line(Name-Ranges) -->
    string(Name_),
    ": ", !,
    sequence(rule_range, " or ", Ranges),
    "\n",
    { atom_codes(Name, Name_) }.

rule_range([X, Y]) --> integer(X), "-", integer(Y).

ticket_line(X) --> sequence(integer, ",", X), "\n".

scanning_error_rate(Rules, Tickets, X) :-
    append(Tickets, All),
    exclude(matches_any_rule(Rules), All, Invalid),
    sum_list(Invalid, X).

valid_value(Ranges, X) :-
    member([A,B], Ranges),
    between(A, B, X).

matches_any_rule([_-Ranges|_], X) :- valid_value(Ranges, X), !.
matches_any_rule([_|Rules], X) :- !, matches_any_rule(Rules, X).

find_fields(Rules, Tickets, Fields) :-
    include(maplist(matches_any_rule(Rules)), Tickets, Valid),
    transpose(Valid, Values),
    maplist(matching_rules(Rules), Values, ValidRules),
    maplist(maplist({Rules}/[A,B]>>nth1(B, Rules, A-_)), ValidRules, EnumValid),
    maplist([A,B]>>element(_, A, B), EnumValid, VS),
    all_distinct(VS),
    maplist({Rules}/[A,B]>>nth1(A, Rules, B-_), VS, Fields).

matching_rules(Rules, Values, X) :-
    convlist({Values}/[Name-Range,Name]>>maplist(valid_value(Range), Values), Rules, X).

example(1) :-
    phrase_from_file(input(Rules, _, Nearby), 'sample-1'),
    scanning_error_rate(Rules, Nearby, 71).

example(2) :-
    phrase_from_file(input(Rules, _, Nearby), 'sample-1'),
    find_fields(Rules, Nearby, [row, class, seat]).

example(3) :-
    phrase_from_file(input(Rules, _, Nearby), 'sample-2'),
    find_fields(Rules, Nearby, [row, class, seat]).

star(1, X) :-
    phrase_from_file(input(Rules, _, Nearby), 'input'),
    scanning_error_rate(Rules, Nearby, X).

star(2, X) :-
    phrase_from_file(input(Rules, Yours, Nearby), 'input'),
    find_fields(Rules, Nearby, Fields),
    pairs_keys_values(Pairs, Fields, Yours),
    include([A-_]>>atom_concat('departure', _, A), Pairs, DeparturesPairs),
    length(DeparturesPairs, 6),
    pairs_values(DeparturesPairs, DeparturesValues),
    foldl([A,B,C]>>(C #= A * B), DeparturesValues, 1, X).

main(_Argv) :-
    example(1),
    example(2),
    example(3),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
