#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(apply)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

input(Data) --> sequence(input_line, Data), eos.

input_line(rule(Bag, Contains)) -->
    bag_desc(Bag),
    " bags contain ",
    contains(Contains),
    ".\n".

contains([]) --> "no other bags".
contains(X) --> sequence(contents, ", ", X).

contents(constraint(1, Bag)) -->
    integer(1),
    " ",
    bag_desc(Bag),
    " bag".

contents(constraint(X, Bag)) -->
    integer(X),
    " ",
    bag_desc(Bag),
    " bags".

bag_desc(bag(Style, Color)) -->
    nonblanks(Style_),
    { string_codes(Style, Style_) },
    " ",
    nonblanks(Color_),
    { string_codes(Color, Color_) }.

load_data(Data, Name) :-
    open(Name, read, Stream),
    phrase_from_stream(input(Data), Stream).

bag_from_rule(rule(Bag, _), Bag).

bag_can_contain(Rules, Bag, Top) :-
    member(rule(Top, Contents), Rules),
    member(constraint(_, Bag), Contents).

bag_can_contain(Rules, Bag, Top) :-
    member(rule(Top, Contents), Rules),
    member(constraint(_, Next), Contents),
    bag_can_contain(Rules, Bag, Next).

can_contain_gold(Rules, X) :-
    maplist(bag_from_rule, Rules, Bags),
    include(bag_can_contain(Rules, bag("shiny", "gold")), Bags, Filtered),
    proper_length(Filtered, X).

constraint_depth(Rules, constraint(N, Bag), X) :-
    bag_depth(Rules, Bag, Y),
    X is N + N * Y.

bag_depth(Rules, Bag, 0) :-
    member(rule(Bag, []), Rules).

bag_depth(Rules, Bag, X) :-
    member(rule(Bag, Constraints), Rules),
    maplist(constraint_depth(Rules), Constraints, Depths),
    sum_list(Depths, X).

example(1) :-
    load_data(Rules, 'sample-1'),
    can_contain_gold(Rules, 4).

example(2) :-
    load_data(Rules, 'sample-1'),
    bag_depth(Rules, bag("shiny", "gold"), 32).

example(3) :-
    load_data(Rules, 'sample-2'),
    bag_depth(Rules, bag("shiny", "gold"), 126).

star(1, X) :-
    load_data(Rules, 'input'),
    can_contain_gold(Rules, X).

star(2, X) :-
    load_data(Rules, 'input'),
    bag_depth(Rules, bag("shiny", "gold"), X).

main(_Argv) :-
    example(1),
    example(2),
    example(3),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
