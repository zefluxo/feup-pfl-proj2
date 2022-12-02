% LIST OPERATIONS %
use_module(library(lists)).

remove_last([], []) :- !, fail.
remove_last([_], []) :- !.
remove_last([X | T], [X | T2]) :-
    remove_last(T, T2).

write_last([X]) :- write(X).
write_last([_|L]) :-
    write_last(L).

take_first([X]) :- X.

% BOARD DRAWING %

tan('q').
dark('a').
light('z').
board([['qq', ' ', ' ', 'qq'],
       [' ', ' ', ' ', ' '],
       [' ', ' ', ' ', ' '],
       ['qq', ' ', ' ', 'qq']]).

draw_board :-
    board(X),
    write('  A B C D '), nl,
    draw_lines(X),
    draw_stacks(X).

draw_lines([]) :-
    nl.

draw_lines([X|L]) :-
    length(L, Length),
    Length1 is (Length - 4)*(-1),
    write(Length1),
    write('|'),
    draw_cells(X),
    draw_lines(L).

draw_cells([]) :- nl,!.
draw_cells([X|L]) :-
    draw_cell(X),
    write('|'),
    draw_cells(L).

draw_cell(X) :-
    atom_chars(X, NX),
    write_last(NX).

draw_stacks([]) :- !.
draw_stacks([X|L]) :-
    length(L, Length),
    Length1 is (Length - 4)*(-1),
    draw_stack_line(X, Length1), nl,
    draw_stacks(L).

draw_stack_line([], R) :- nl, !.
draw_stack_line([X|L], R) :-
    length(L, Length),
    C1 is 64+(Length - 4)*(-1),
    char_code(C, C1),
    draw_stack(X, R, C),
    tab(1),
    draw_stack_line(L, R).

draw_stack(X, R, C) :-
    write(C),write(R),write(' = ['), write(X), write(']').