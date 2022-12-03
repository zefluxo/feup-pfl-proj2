% LIST OPERATIONS %
use_module(library(lists)).

remove_last([], []) :- !, fail.
remove_last([_], []) :- !.
remove_last([X | T], [X | T2]) :-
    remove_last(T, T2).

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

error_msg :- write('\n\nInvalid input! Please try again...\n\n').
exit_msg :- write('\n\nGoodbye!\n\n').

% MAIN LOOP/STATES %

mode(h/h).

play :-
    menu(main_menu).

menu(main_menu) :- 
    main_menu,
    read(Opt),
    (
        Opt =:= 1 ->
            menu(play);
        Opt =:= 2 -> 
            exit_msg,
            fail;
        error_msg,
        menu(main_menu)
    ).

menu(quit) :- quit_menu.
menu(play) :- play_menu.

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
    assert(mode(Mode)),
    start_game, !.

start_game :-
    initial_state(GameState),
    display_game(GameState),
    gamemode(StartPlayer/_),
    run_game(GameState/StartPlayer).

validate_mode(P1/P2) :-
    validate_player(P1), validate_player(P2).

validate_player(h).
validate_player(c-Level) :-
    1 is Level; 2 is Level.

run_game(GameState-Player) :-
    write(GameState-Player).

initial_state(Board-dark) :- .

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