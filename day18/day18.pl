#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

input(Expr, X) --> sequence(input_line(Expr), X), eos.

input_line(Expr, X) --> call(Expr, X), "\n".

opp(+) --> "+".
opp(*) --> "*".

flat_term_expr(X) --> integer(X).
flat_term_expr(X) --> "(", whites, flat_expr(X), ")".

flat_expr(Z) --> flat_term_expr(X), whites, flat_expr_1(X, Z).

flat_expr_1(X, Z) -->
    opp(Opp),
    whites,
    flat_term_expr(Y),
    whites,
    flat_expr_1([Opp, X, Y], Z).
flat_expr_1(X, X) --> [].

adv_term_expr(X) --> integer(X).
adv_term_expr(X) --> "(", whites, adv_expr(X), ")".

adv_add_expr(Z) --> adv_term_expr(X), whites, adv_add_expr_1(X, Z).

adv_add_expr_1(X, Z) -->
    "+",
    whites,
    adv_term_expr(Y),
    whites,
    adv_add_expr_1([+, X, Y], Z).
adv_add_expr_1(X, X) --> [].

adv_expr(Z) --> adv_add_expr(X), whites, adv_expr_1(X, Z).

adv_expr_1(X, Z) -->
    "*",
    whites,
    adv_add_expr(Y),
    whites,
    adv_expr_1([*, X, Y], Z).
adv_expr_1(X, X) --> [].

eval_expr(X, X) :- integer(X).

eval_expr([+, X, Y], Z) :-
    eval_expr(X, X_),
    eval_expr(Y, Y_),
    Z #= X_ + Y_.

eval_expr([*, X, Y], Z) :-
    eval_expr(X, X_),
    eval_expr(Y, Y_),
    Z #= X_ * Y_.

example(1) :-
    phrase_from_file(input(flat_expr, Exprs), 'sample'),
    maplist(eval_expr, Exprs, [51,26,437,12240,13632]).

example(2) :-
    phrase_from_file(input(adv_expr, Exprs), 'sample'),
    maplist(eval_expr, Exprs, [51,46,1445,669060,23340]).

star(1, X) :-
    phrase_from_file(input(flat_expr, Exprs), 'input'),
    maplist(eval_expr, Exprs, Values),
    sumlist(Values, X).

star(2, X) :-
    phrase_from_file(input(adv_expr, Exprs), 'input'),
    maplist(eval_expr, Exprs, Values),
    sumlist(Values, X).

main(_Argv) :-
    example(1),
    example(2),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
