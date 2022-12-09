
% LIST OPERATIONS %
use_module(library(lists)).

write_last([X]) :- write(X).
write_last([_|L]) :-
    write_last(L).

% FILE PROCESSING %

read_file(Stream,[]) :-
    at_end_of_stream(Stream).
read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream,Codes),
    atom_chars(X, Codes),
    read_file(Stream,L), !.

write_file([]) :- !.
write_file([X|L]) :-
    write(X), nl, write_file(L).

% ERROR MESSAGES %

error_msg :- write('\n\nInvalid input! Please try again...\n\n').
exit_msg :- write('\n\nGoodbye!\n\n').

% MENUS %

main_menu :-
  open('src/main_menu.txt', read, Str),
  read_file(Str,Lines),
  close(Str),
  write_file(Lines).

play_menu :-
    write('How would you like to play? Please input the role of each player in the format [P1/P2.].\nAvailable roles:\n[h] - Human player;\n[c-1] - Easy computer;\n[c-2] - Hard computer;\n\n'),
    read(Mode),
    validate_mode(Mode),
    retract(mode(_)),
    assert(mode(Mode)), !.

quit_menu :-
    write('\n\nThank you for playing. Goodbye!\n\n'),
    fail. 

display_game(Board-_) :-
    write(Board),
    draw_board(Board).

validate_mode(P1/P2) :-
    validate_player(P1), validate_player(P2).

validate_player(h).
validate_player(c-Level) :-
    1 is Level; 2 is Level.

finish(Winner) :-
    format('YOU ~n1!!! LEZZZZZ GOOOOOO!!!!', [Winner]),
    fail.

% BOARD DRAWING %

draw_board(Board) :-
    write('  A B C D '), nl,
    draw_lines(Board),
    draw_stacks(Board).

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

draw_stack_line([], _) :- nl, !.
draw_stack_line([X|L], R) :-
    length(L, Length),
    C1 is 64+(Length - 4)*(-1),
    char_code(C, C1),
    draw_stack(X, R, C),
    tab(1),
    draw_stack_line(L, R).

draw_stack(X, R, C) :-
    write(C),write(R),write(' = ['), write(X), write(']').

% Movement %

get_move(GameState, Player, Move).
get_piece_placement(GameState, Player, PGameState).