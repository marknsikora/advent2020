#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(ordsets)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(yall)).

:- initialization(main, main).

input(Data) --> sequence(input_line, Data), eos, !.

input_line(X) --> sequence(seating_char, X), "\n".

seating_char(seat) --> "L".
seating_char(floor) --> ".".

grid_to_points(Grid, Seats) :-
    foldl(grid_row, Grid, state(0, []), state(_, Seats)).

grid_row(Row, state(Y, Seats), state(Y_, NextSeats)) :-
    Y_ is Y + 1,
    foldl(grid_seat(Y), Row, state(0, Seats), state(_, NextSeats)).

grid_seat(_, floor, state(X, Seats), state(X_, Seats)) :-
    X_ is X + 1.

grid_seat(Y, seat, state(X, Seats), state(X_, [[X, Y]|Seats])) :-
    X_ is X + 1.

point_neighbours(Seats, [X, Y], Neighbours) :-
    NextX is X + 1,
    PrevX is X - 1,
    NextY is Y + 1,
    PrevY is Y - 1,
    % This is an ordset
    L = [
        [PrevX, PrevY],
        [PrevX, Y],
        [PrevX, NextY],
        [X,     PrevY],
        [X,     NextY],
        [NextX, PrevY],
        [NextX, Y],
        [NextX, NextY]
    ],
    % Pre-filter the neighbours here to save checks later
    ord_intersect(L, Seats, Neighbours).

point_sightlines(Seats, Point, Neighbours) :-
    L = [
        [-1, -1],
        [-1, 0],
        [-1, 1],
        [0, -1],
        [0, 1],
        [1, -1],
        [1, 0],
        [1, 1]
    ],
    convlist(point_visible(Seats, Point), L, Neighbours).

point_visible(Seats, [X, Y], [A, B], [M, N]) :-
    tuples_in([[M, N]], Seats),
    M #= X + Q * A,
    N #= Y + Q * B,
    Q in 1..sup,
    label([Q]),
    !.

step(Seats, Threshold, Occupied, NextOccupied) :-
    convlist(rule(Threshold, Occupied), Seats, Filtered),
    pairs_keys_values(Pairs, Filtered, Filtered),
    list_to_assoc(Pairs, NextOccupied),
    !.

rule(Threshold, Occupied, Neighbours-Seat, Seat) :-
    include({Occupied}/[Y]>>get_assoc(Y, Occupied, _), Neighbours, OccupiedNeighbours),
    proper_length(OccupiedNeighbours, L),
    (   get_assoc(Seat, Occupied, _)
    ->  L < Threshold
    ;   ord_empty(OccupiedNeighbours)
    ).

find_stable(Seats, Neighbour, Threshold, X) :-
    list_to_ord_set(Seats, Ordered),
    map_list_to_pairs(call(Neighbour, Ordered), Ordered, SeatNeighbours),
    empty_assoc(Occupied),
    find_stable_(SeatNeighbours, Threshold, Occupied, X).

find_stable_(Seats, Threshold, Occupied, X) :-
    step(Seats, Threshold, Occupied, NextOccupied),
    assoc_to_keys(Occupied, OccupiedList),
    (   Occupied == NextOccupied
    ->  proper_length(OccupiedList, X)
    ;   find_stable_(Seats, Threshold, NextOccupied, X)
    ).

example(1) :-
    phrase_from_file(input(Grid), 'sample'),
    grid_to_points(Grid, Points),
    find_stable(Points, point_neighbours, 4, 37).

example(2) :-
    phrase_from_file(input(Grid), 'sample'),
    grid_to_points(Grid, Points),
    find_stable(Points, point_sightlines, 5, 26).

star(1, X) :-
    phrase_from_file(input(Grid), 'input'),
    grid_to_points(Grid, Points),
    find_stable(Points, point_neighbours, 4, X).

star(2, X) :-
    phrase_from_file(input(Grid), 'input'),
    grid_to_points(Grid, Points),
    find_stable(Points, point_sightlines, 5, X).

main(_Argv) :-
    example(1),
    example(2),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
