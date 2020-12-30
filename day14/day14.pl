#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

input(Data) --> sequence(instruction_line, Data), eos.

instruction_line(X) --> instruction(X), "\n".

mask_char(0) --> "0".
mask_char(1) --> "1".
mask_char(x) --> "X".

instruction(mask(X)) -->
    "mask",
    whites,
    "=",
    whites,
    sequence(mask_char, X).

instruction(mem(X, Y)) -->
    "mem",
    "[",
    integer(X),
    "]",
    whites,
    "=",
    whites,
    integer(Y).

simulate(Insts, Decoder, X) :-
    empty_assoc(EmptyMem),
    foldl(Decoder, Insts, state(EmptyMem, -), state(NewMem, _)),
    assoc_to_values(NewMem, Values),
    sum_list(Values, X).

execute_data(mask(X), state(Mem, _), state(Mem, Mask)) :-
    foldl(data_mask, X, mask(0, 0), Mask).

execute_data(mem(X, Y), state(Mem, mask(A, B)), state(NewMem, mask(A, B))) :-
    Y_ #= Y \/ A /\ \B,
    put_assoc(X, Mem, Y_, NewMem).

data_mask(x, mask(A, B), mask(A_, B_)) :-
    A_ #= A << 1,
    B_ #= B << 1.

data_mask(1, mask(A, B), mask(A_, B_)) :-
    A_ #= (A << 1) + 1,
    B_ #= B << 1.

data_mask(0, mask(A, B), mask(A_, B_)) :-
    A_ #= A << 1,
    B_ #= (B << 1) + 1.

execute_memory(mask(X), state(Mem, _), state(Mem, Mask)) :-
    findall(M, foldl(memory_mask, X, mask(0, 0), M), Mask).

execute_memory(mem(X, Y), state(Mem, Mask), state(NewMem, Mask)) :-
    foldl(set_memory_mask(X, Y), Mask, Mem, NewMem).

set_memory_mask(X, Y, mask(A, B), Mem, NewMem) :-
    X_ #= A \/ (\B /\ X),
    put_assoc(X_, Mem, Y, NewMem).

memory_mask(0, mask(A, B), mask(A_, B_)) :-
    A_ #= A << 1,
    B_ #= B << 1.

memory_mask(1, mask(A, B), mask(A_, B_)) :-
    A_ #= (A << 1) + 1,
    B_ #= B << 1.

memory_mask(x, mask(A, B), mask(A_, B_)) :-
    A_ #= A << 1,
    B_ #= (B << 1) + 1.

memory_mask(x, mask(A, B), mask(A_, B_)) :-
    A_ #= (A << 1) + 1,
    B_ #= (B << 1) + 1.

example(1) :-
    phrase_from_file(input(Insts), 'sample-1'),
    simulate(Insts, execute_data, 165).

example(2) :-
    phrase_from_file(input(Insts), 'sample-2'),
    simulate(Insts, execute_memory, 208).

star(1, X) :-
    phrase_from_file(input(Insts), 'input'),
    simulate(Insts, execute_data, X).

star(2, X) :-
    phrase_from_file(input(Insts), 'input'),
    simulate(Insts, execute_memory, X).

main(_Argv) :-
    example(1),
    example(2),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
