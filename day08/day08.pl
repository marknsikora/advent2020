#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(apply)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

input(Data) --> sequence(input_line, Data), eos.

input_line(inst(Op, N)) -->
    nonblanks(Op_),
    { string_codes(Op, Op_) },
    " ",
    integer(N),
    "\n".

find_loop(Insts, X) :- find_loop(Insts, [], 0, 0, X).

find_loop(_Insts, Visited, IP, Acc, Acc) :-
    member(IP, Visited).

find_loop(Insts, Visited, IP, Acc, X) :-
    nth0(IP, Insts, Inst),
    run(Inst, IP, Acc, IP_, Acc_),
    find_loop(Insts, [IP|Visited], IP_, Acc_, X).

run(inst("acc", N), IP, Acc, IP_, Acc_) :-
    Acc_ is Acc + N,
    IP_ is IP + 1.

run(inst("jmp", N), IP, Acc, IP_, Acc) :-
    IP_ is IP + N.

run(inst("nop", _), IP, Acc, IP_, Acc) :-
    IP_ is IP + 1.

terminate(Insts, X) :- terminate(Insts, [], 0, 0, X).

terminate(Insts, _, IP, Acc, Acc) :-
    \+ nth0(IP, Insts, _).

terminate(Insts, Visited, IP, Acc, X) :-
    \+ member(IP, Visited),
    nth0(IP, Insts, Inst),
    run(Inst, IP, Acc, IP_, Acc_),
    terminate(Insts, [IP|Visited], IP_, Acc_, X).

rewrite(inst("jmp", N), inst("nop", N)).
rewrite(inst("nop", N), inst("jmp", N)).

example(1) :-
    phrase_from_file(input(Insts), 'sample'),
    find_loop(Insts, 5).

example(2) :-
    phrase_from_file(input(Insts), 'sample'),
    select(A, Insts, B, Fixed),
    rewrite(A, B),
    terminate(Fixed, 8).

star(1, X) :-
    phrase_from_file(input(Insts), 'input'),
    find_loop(Insts, X).

star(2, X) :-
    phrase_from_file(input(Insts), 'input'),
    select(A, Insts, B, Fixed),
    rewrite(A, B),
    terminate(Fixed, X).

main :-
    example(1),
    example(2),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
