#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(yall)).

:- initialization(main, main).

input(X) -->
    sequence(digit, Codes),
    { maplist([A,B]>>number_codes(B, [A]), Codes, X) },
    "\n",
    eos,
    !.

cup_game(Head0, Cups0, Head, Cups) :-
    % Get next few cups in sequence
    get_assoc(Head0, Cups0, A),
    get_assoc(A, Cups0, B),
    get_assoc(B, Cups0, C),
    get_assoc(C, Cups0, D),
    % Remove [A,B,C] from sequence
    put_assoc(Head0, Cups0, D, Cups1),
    % Get index to insert cups
    I0 #= Head0 - 1,
    cups_destination(Cups1, [A,B,C], I0, I),
    get_assoc(I, Cups1, J),
    % Insert cups
    put_assoc(I, Cups1, A, Cups2),
    put_assoc(C, Cups2, J, Cups3),
    % Set new values
    Cups = Cups3,
    Head = D.

cups_destination(Cups, Removed, I0, I) :-
    member(I0, Removed),
    I1 #= I0 - 1,
    cups_destination(Cups, Removed, I1, I).

cups_destination(Cups, _, I, I) :-
    get_assoc(I, Cups, _).

cups_destination(Cups, Removed, I0, I) :-
    min_assoc(Cups, Min, _),
    I0 #=< Min,
    max_assoc(Cups, I1, _),
    cups_destination(Cups, Removed, I1, I).

cups_destination(Cups, Removed, I0, I) :-
    I1 #= I0 - 1,
    cups_destination(Cups, Removed, I1, I).

cup_rounds(N, Cups0, Cups) :-
    [Head|_] = Cups0,
    next_in_sequence(Cups0, Pairs),
    list_to_assoc(Pairs, Cups1),
    cup_rounds(N, Head, Cups1, Cups).

cup_rounds(0, _, Cups, Cups).
cup_rounds(N0, Head0, Cups0, Cups) :-
    N0 #> 0,
    N #= N0 - 1,
    cup_game(Head0, Cups0, Head, Cups1),
    !,
    cup_rounds(N, Head, Cups1, Cups).

next_in_sequence([X|XS], YS) :- next_in_sequence(X, [X|XS], YS).

next_in_sequence(A, [X0,X1|XS], [X0-X1|YS]) :- next_in_sequence(A, [X1|XS], YS).
next_in_sequence(A, [X0], [X0-A]).
next_in_sequence(_, [], []).

sequence_from(Start, Assoc, XS) :- sequence_from(Start, Assoc, Start, XS).
sequence_from(Start, Assoc, X0, []) :-
    get_assoc(X0, Assoc, Start).
sequence_from(Start, Assoc, X0, [X1|XS]) :-
    get_assoc(X0, Assoc, X1),
    sequence_from(Start, Assoc, X1, XS).

extend_cups(Cups0, Cups) :-
    max_list(Cups0, Max),
    Lower #= Max + 1,
    numlist(Lower, 1_000_000, Rest),
    append(Cups0, Rest, Cups).

example(1) :-
    phrase_from_file(input(Cups), 'sample'),
    cup_rounds(100, Cups, Final),
    sequence_from(1, Final, [6,7,3,8,4,5,2,9]).

example(2) :-
    phrase_from_file(input(Cups), 'sample'),
    extend_cups(Cups, AllCups),
    cup_rounds(10_000_000, AllCups, Final),
    get_assoc(1, Final, A),
    get_assoc(A, Final, B),
    149245887792 #= A * B.

star(1, X) :-
    phrase_from_file(input(Cups), 'input'),
    cup_rounds(100, Cups, Final),
    sequence_from(1, Final, X).

star(2, X) :-
    phrase_from_file(input(Cups), 'input'),
    extend_cups(Cups, AllCups),
    cup_rounds(10_000_000, AllCups, Final),
    get_assoc(1, Final, A),
    get_assoc(A, Final, B),
    X #= A * B.

main(_Argv) :-
    example(1),
    example(2),
    star(1, X),
    portray_clause(X),
    star(2, Y),
    format('~d~n', [Y]).
