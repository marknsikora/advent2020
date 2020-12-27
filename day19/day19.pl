#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

input(Rules, Messages) -->
    sequence(rule_line, Rules),
    "\n",
    sequence(message_line, Messages),
    eos.

rule_line(ID-Rules) -->
    integer(ID),
    ": ",
    rule_part(Rules),
    "\n".

rule_part(codes(X)) --> "\"", string_without("\"", X), "\"", !.
rule_part(X) --> sequence(rule_list, rule_separator, X).

rule_separator --> whites, "|", whites.

rule_list(X) --> sequence(integer, whites, X).

message_line(X) --> string_without("\n", X), "\n".

match_rule(Rules, Rule) -->
    { get_assoc(Rule, Rules, codes(Codes)) },
    Codes.

match_rule(Rules, Rule) -->
    { get_assoc(Rule, Rules, SubRules), member(SubRule, SubRules) },
    foldl(match_rule(Rules), SubRule).

matching_messages_count(Rules, Messages, X) :-
    include(phrase(match_rule(Rules, 0)), Messages, Valid),
    proper_length(Valid, X).

fix_rules(Rules, FixedRules) :-
    put_assoc(8, Rules, [[42],[42,8]], Rules4),
    put_assoc(11, Rules4, [[42,31],[42,11,31]], FixedRules).

example(1) :-
    phrase_from_file(input(Rules, Messages), 'sample-1'),
    list_to_assoc(Rules, RulesAssoc),
    matching_messages_count(RulesAssoc, Messages, 2).

example(2) :-
    phrase_from_file(input(Rules, Messages), 'sample-2'),
    list_to_assoc(Rules, RulesAssoc),
    matching_messages_count(RulesAssoc, Messages, 3).

example(3) :-
    phrase_from_file(input(Rules, Messages), 'sample-2'),
    list_to_assoc(Rules, RulesAssoc),
    fix_rules(RulesAssoc, FixedRules),
    matching_messages_count(FixedRules, Messages, 12).

star(1, X) :-
    phrase_from_file(input(Rules, Messages), 'input'),
    list_to_assoc(Rules, RulesAssoc),
    matching_messages_count(RulesAssoc, Messages, X).

star(2, X) :-
    phrase_from_file(input(Rules, Messages), 'input'),
    list_to_assoc(Rules, RulesAssoc),
    fix_rules(RulesAssoc, FixedRules),
    matching_messages_count(FixedRules, Messages, X).

main(_Argv) :-
    example(1),
    example(2),
    example(3),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
