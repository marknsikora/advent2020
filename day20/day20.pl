#!/usr/bin/env swipl
% vim: ft=prolog

:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

input(X) --> sequence(tile, "\n", X), eos.

tile(ID-Img) -->
    "Tile ", integer(ID), ":\n",
    img(Img).

img(X) --> sequence(img_line, X).
img_line(X) --> sequence(pixel, X), "\n".

pixel(on) --> "#".
pixel(off) --> ".".

rotate(Tile, Tile).
rotate(Tile0, Tile) :- maplist(reverse, Tile0, Tile).
rotate(Tile0, Tile) :- transpose(Tile0, Tile).
rotate(Tile0, Tile) :- transpose(Tile0, Tile1), reverse(Tile1, Tile).
rotate(Tile0, Tile) :- reverse(Tile0, Tile).
rotate(Tile0, Tile) :- reverse(Tile0, Tile1), maplist(reverse, Tile1, Tile).
rotate(Tile0, Tile) :- transpose(Tile0, Tile1), maplist(reverse, Tile1, Tile).
rotate(Tile0, Tile) :- transpose(Tile0, Tile1), maplist(reverse, Tile1, Tile2), reverse(Tile2, Tile).

empty_tile(Tile0, Tile) :-
    same_length(Tile0, Tile),
    maplist(same_length, Tile0, Tile).

head([X|_], X).

tail([X], X).
tail([_|XS], Y) :- tail(XS, Y).

trim(X, Y) :- append([_|Y], [_], X).

row_match(Left, Right) :-
    maplist(tail, Left, Crease),
    maplist(head, Right, Crease).

col_match(Top, Bottom) :-
    tail(Top, Crease),
    head(Bottom, Crease).

build_img(_, [], []).
build_img(Tiles0, [Row|Rows], Imgs) :-
    [_-T|_] = Tiles0,
    same_length(Row, EmptyRow),
    maplist(empty_tile(T), EmptyRow),
    build_img(Tiles0, [Row|Rows], EmptyRow, Imgs).

build_img(_, [], _, []).
build_img(Tiles0, [Row|Rows], Img0, [Img|Imgs]) :-
    [_-T|_] = Tiles0,
    empty_tile(T, E),
    build_row(Tiles0, Row, [E|Img], Img0, Tiles),
    build_img(Tiles, Rows, Img, Imgs).

% There is probably some really clever way to generalize this to the
% N-dimentional case instead of hard-coding on 2D
build_row(Tiles0, [X|XS], [T0,T1|TS], [U|US], Tiles) :-
    % Shape T1
    empty_tile(T0, T1),
    % T1 sides match left and up
    row_match(T0, T1),
    col_match(U, T1),
    % Some rotated version of T1 exists in remaining tiles
    rotate(T1, TR),
    select(X-TR, Tiles0, Tiles1),
    % Next tile
    build_row(Tiles1, XS, [T1|TS], US, Tiles).
build_row(Tiles, [], _, _, Tiles).

trim_tile -->
    trim,
    maplist(trim).

combine_imgs -->
    maplist(maplist(trim_tile)),
    maplist(transpose),
    maplist(maplist(append)),
    append,
    !,
    rotate.

mask_match(Points, Mask, Match) :-
    maplist(maplist([A,B,C]>>(C #= A + B), Offset), Mask, Match),
    tuples_in(Match, Points),
    label(Offset).

img_points(Img, Points) :- findall([X, Y], (nth1(Y, Img, Row), nth1(X, Row, on)), Points).

find_sea_monsters(Imgs, X) :-
    phrase_from_file(img(Monster), 'sea-monster'),
    img_points(Monster, MonsterPoints),
    !, % Avoid re-parsing monster
    combine_imgs(Imgs, FullImg),
    img_points(FullImg, ImgPoints),
    findall(Match, mask_match(ImgPoints, MonsterPoints, Match), Matches),
    proper_length(Matches, L),
    L #> 0,
    append(Matches, AllMatches),
    list_to_set(AllMatches, MatchesSet),
    subtract(ImgPoints, MatchesSet, Turbulent),
    proper_length(Turbulent, X).

example(1) :-
    phrase_from_file(input(Tiles), 'sample'),
    Grid = [
        [A,_,B],
        [_,_,_],
        [C,_,D]
    ],
    build_img(Tiles, Grid, _),
    20899048083289 #= A * B * C * D.

example(2) :-
    phrase_from_file(input(Tiles), 'sample'),
    length(Grid, 3), maplist([A]>>length(A, 3), Grid),
    build_img(Tiles, Grid, Imgs),
    find_sea_monsters(Imgs, 273).

star(1, X) :-
    phrase_from_file(input(Tiles), 'input'),
    Grid = [
        [A,_,_,_,_,_,_,_,_,_,_,B],
        [_,_,_,_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_,_,_,_],
        [C,_,_,_,_,_,_,_,_,_,_,D]
    ],
    build_img(Tiles, Grid, _),
    X #= A * B * C * D.

star(2, X) :-
    phrase_from_file(input(Tiles), 'input'),
    length(Grid, 12), maplist([A]>>length(A, 12), Grid),
    build_img(Tiles, Grid, Imgs),
    find_sea_monsters(Imgs, X).

main(_Argv) :-
    example(1),
    example(2),
    star(1, X),
    format('~d~n', [X]),
    star(2, Y),
    format('~d~n', [Y]).
