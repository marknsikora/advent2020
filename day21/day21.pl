#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

input(X) --> sequence(food_line, X), eos, !.

word(X) -->
    sequence(alpha_to_lower, Codes),
    { proper_length(Codes, L), L #> 0, atom_codes(X, Codes) }.

food_line(X-Y) -->
    ingredients(X),
    allergens(Y),
    "\n".

ingredients(X) --> sequence(word, " ", X).

allergens(X) --> sequence("(contains ", word, ", ", ")", X).

get_unique(Lists, Unique, Enum) :-
    append(Lists, All),
    sort(All, Unique),
    maplist(maplist({Unique}/[A,B]>>nth1(A, Unique, B)), Enum, Lists).

foods_integer_domain(Foods, UniqueIngredients, UniqueAllergens, X) :-
    pairs_keys_values(Foods, Ingredients, Allergens),
    get_unique(Ingredients, UniqueIngredients, EnumIngredients),
    get_unique(Allergens, UniqueAllergens, EnumAllergens),
    pairs_keys_values(X, EnumIngredients, EnumAllergens).

ingredient_allergen_combinations(Ingredients-Allergens, X) :-
    findall([A,B], (member(A, Allergens), member(B, Ingredients)), X).

identify_allergens(Foods, Z) :-
    foods_integer_domain(Foods, Ingredients, Allergens, EnumFoods),
    pairs_values(EnumFoods, EnumAllergens),
    % Generate tuples of all possible candidates
    maplist(ingredient_allergen_combinations, EnumFoods, FoodCombinations),
    % Generate lookup tuples
    proper_length(Allergens, LAllergens),
    numlist(1, LAllergens, AllergenIDS),
    maplist([A,B,C]>>(C = [A,B]), AllergenIDS, VS, Mappings),
    maplist(maplist({Mappings}/[A,B]>>nth1(A, Mappings, B)), EnumAllergens, Tuples),
    % Find Solution
    maplist(tuples_in, Tuples, FoodCombinations),
    all_distinct(VS),
    % Mapping back to atom domain
    maplist({Ingredients}/[A,B]>>nth1(A, Ingredients, B), VS, AllergenIngredients),
    pairs_keys_values(Z, Allergens, AllergenIngredients).

count_allergen_free(Foods, X) :-
    identify_allergens(Foods, AllergenIngredients),
    pairs_keys(Foods, Ingredients),
    pairs_values(AllergenIngredients, BadIngredients),
    append(Ingredients, AllIngredients),
    exclude({BadIngredients}/[A]>>memberchk(A, BadIngredients), AllIngredients, AllergenFree),
    proper_length(AllergenFree, X).

bad_ingredients(Foods, X) :-
    identify_allergens(Foods, AllergenIngredients),
    pairs_values(AllergenIngredients, X).

example(1) :-
    phrase_from_file(input(Foods), 'sample'),
    count_allergen_free(Foods, 5).

example(2) :-
    phrase_from_file(input(Foods), 'sample'),
    bad_ingredients(Foods, [mxmxvkd,sqjhc,fvjkl]).

star(1, X) :-
    phrase_from_file(input(Foods), 'input'),
    count_allergen_free(Foods, X).

star(2, X) :-
    phrase_from_file(input(Foods), 'input'),
    bad_ingredients(Foods, X).

main(_Argv) :-
    example(1),
    example(2),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    portray_clause(Y).
