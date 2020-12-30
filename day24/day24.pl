#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

input(X) --> sequence(input_line, X), eos.
input_line(X) --> sequence(tile, X), "\n".

tile(e)  --> "e".
tile(se) --> "se".
tile(sw) --> "sw".
tile(w)  --> "w".
tile(nw) --> "nw".
tile(ne) --> "ne".

% Grid layout, X index
%  0 1
% 0 1 2
%  1 2

path(Z) --> path([0, 0], Z).

path([X0, Y0], Z) --> [e],  {X #= X0 + 1, Y #= Y0    }, path([X, Y], Z).
path([X0, Y0], Z) --> [se], {X #= X0 + 1, Y #= Y0 - 1}, path([X, Y], Z).
path([X0, Y0], Z) --> [sw], {X #= X0,     Y #= Y0 - 1}, path([X, Y], Z).
path([X0, Y0], Z) --> [w],  {X #= X0 - 1, Y #= Y0    }, path([X, Y], Z).
path([X0, Y0], Z) --> [nw], {X #= X0 - 1, Y #= Y0 + 1}, path([X, Y], Z).
path([X0, Y0], Z) --> [ne], {X #= X0,     Y #= Y0 + 1}, path([X, Y], Z).

path(Z, Z) --> [].

flip_tiles(X, [X|YS], YS).
flip_tiles(X, YS, [X|YS]).

flipped_tiles(Directions, X) :-
    maplist([A,B]>>phrase(path(B), A), Directions, Locations),
    msort(Locations, Sorted),
    foldl(flip_tiles, Sorted, [], X).

tile_neighbour(A, B) :- phrase(path(A, B), [_]).
tile_neighbours(A, B) :- findall(X, tile_neighbour(A, X), B).

run(Tiles, N, Z) :-
    pairs_keys_values(Pairs, Tiles, Tiles),
    list_to_assoc(Pairs, Start),
    run_(Start, N, End),
    assoc_to_keys(End, Z).

run_(Blacks, 0, Blacks).
run_(Blacks, N0, Z) :-
    N0 #> 0,
    N #= N0 - 1,
    step(Blacks, NextBlacks),
    run_(NextBlacks, N, Z).

step(Blacks, NextBlacks) :-
    findall(X, (gen_assoc(T, Blacks, _), tile_neighbour(T, X)), CheckTiles),
    sort(CheckTiles, Sorted),
    include(rule(Blacks), Sorted, Next),
    pairs_keys_values(Pairs, Next, Next),
    list_to_assoc(Pairs, NextBlacks),
    !.

rule(Blacks, Tile) :-
    tile_neighbours(Tile, Neighbours),
    include({Blacks}/[A]>>get_assoc(A, Blacks, _), Neighbours, BlackNeighbours),
    proper_length(BlackNeighbours, L),
    (   get_assoc(Tile, Blacks, _)
    ->  L #=< 2
    ;   L #= 2
    ).

example(1) :-
    phrase_from_file(input(Directions), 'sample'),
    flipped_tiles(Directions, Flipped),
    proper_length(Flipped, 10).

example(2) :-
    phrase_from_file(input(Directions), 'sample'),
    flipped_tiles(Directions, Flipped),
    run(Flipped, 100, End),
    proper_length(End, 2208).

star(1, X) :-
    phrase_from_file(input(Directions), 'input'),
    flipped_tiles(Directions, Flipped),
    proper_length(Flipped, X).

star(2, X) :-
    phrase_from_file(input(Directions), 'input'),
    flipped_tiles(Directions, Flipped),
    run(Flipped, 100, End),
    proper_length(End, X).

main(_Argv) :-
    example(1),
    example(2),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
