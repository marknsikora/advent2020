#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

alpha(C) -->
    [C],
    { code_type(C, alpha) }.

input(Data) -->
    sequence(entry, "\n", Data),
    blanks,
    eos.

entry(X) -->
    sequence(entry_line, Y),
    { append(Y, X) }.

entry_line(X) -->
    sequence(field, " ", X),
    { member(_, X) },
    "\n".

field(pair(Key, Value)) -->
    sequence(alpha, Key_),
    { string_codes(Key, Key_) },
    ":",
    nonblanks(Value).

validate(X) :-
    member(pair("byr", _), X),
    member(pair("iyr", _), X),
    member(pair("eyr", _), X),
    member(pair("hgt", _), X),
    member(pair("hcl", _), X),
    member(pair("ecl", _), X),
    member(pair("pid", _), X).

revalidate(X) :-
    member(pair("byr", Byr), X), phrase(byr, Byr),
    member(pair("iyr", Iyr), X), phrase(iyr, Iyr),
    member(pair("eyr", Eyr), X), phrase(eyr, Eyr),
    member(pair("hgt", Hgt), X), phrase(hgt, Hgt),
    member(pair("hcl", Hcl), X), phrase(hcl, Hcl),
    member(pair("ecl", Ecl), X), phrase(ecl, Ecl),
    member(pair("pid", Pid), X), phrase(pid, Pid).

byr --> integer(X), { X >= 1920, X =< 2002 }.
iyr --> integer(X), { X >= 2010, X =< 2020 }.
eyr --> integer(X), { X >= 2020, X =< 2030 }.

hgt --> integer(X), "cm", { X >= 150, X =< 193 }.
hgt --> integer(X), "in", { X >= 59, X =< 76 }.

hcl --> "#", sequence(xdigit, X), { proper_length(X, 6) }.

ecl --> "amb".
ecl --> "blu".
ecl --> "brn".
ecl --> "gry".
ecl --> "grn".
ecl --> "hzl".
ecl --> "oth".

pid --> sequence(digit, X), { proper_length(X, 9) }.

example(1) :-
    phrase_from_file(input(Entries), 'sample'),
    include(validate, Entries, Valid),
    proper_length(Valid, 2).

example(2) :-
    phrase_from_file(input(Entries), 'invalid'),
    include(revalidate, Entries, Valid),
    proper_length(Valid, 0).

example(3) :-
    phrase_from_file(input(Entries), 'valid'),
    include(revalidate, Entries, Valid),
    proper_length(Valid, 4).

star(1, X) :-
    phrase_from_file(input(Entries), 'input'),
    include(validate, Entries, Valid),
    proper_length(Valid, X).

star(2, X) :-
    phrase_from_file(input(Entries), 'input'),
    include(revalidate, Entries, Valid),
    proper_length(Valid, X).

main(_Argv) :-
    example(1),
    example(2),
    example(3),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
