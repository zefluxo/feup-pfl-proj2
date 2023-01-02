
% LIST OPERATIONS %
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).

% ERROR MESSAGES %

% error_msg/0
% Outputs a message signaling invalid inputs
error_msg :- write('\n\nInvalid input! Please try again...\n\n').

% exit_msg/0
% Outputs the program's exit message
exit_msg :- write('\n\nGoodbye!\n\n').

% MENUS %

% main_menu/0
% Simple function to draw the main menu
main_menu :-
    write(' _______  _______  _     _  _______  ___      _______\n'),
    write('|       ||   _   || | _ | ||   _   ||   |    |       |\n'),
    write('|   _   ||  |_|  || || || ||  |_|  ||   |    |    ___|\n'),
    write('|  | |  ||       ||       ||       ||   |    |   |___ \n'),
    write('|  |_|  ||       ||       ||       ||   |___ |    ___|\n'),
    write('|      | |   _   ||   _   ||   _   ||       ||   |___ \n'),
    write('|____||_||__| |__||__| |__||__| |__||_______||_______|\n'),
    nl,
    write('1. - Play\n'),
    write('2. - Quit\n').

% validate_mode(+P1/P2)
% Verifies whether the input players are valid or not
validate_mode(P1/P2) :-
    ground(P1), ground(P2),
    validate_player(P1), validate_player(P2).

% validate_player(+Player)
% Verifies if the given Player is valid
validate_player(h).
validate_player(c-Level) :-
    1 is Level; 2 is Level.
    
% play_menu(+Mode)
% Outputs the play menu, parses and checks the user input for players
% and updates the game_mode to let the program know who is playing
play_menu(Mode) :-
    write('\n\nStacks are represented as "bottom -> top"\n'),
    write('How would you like to play? Please input the role of each player in the format [P1/P2.].\nAvailable roles:\n[h] - Human player;\n[c-1] - Easy computer;\n[c-2] - Hard computer;\n\n'),
    read(NMode),
    (
        validate_mode(NMode) -> 
        (
            Mode = NMode,!
        );
        write('Not a valid gamemode! Try again...\n'),
        !, play_menu(Mode)
    ).

% quit_menu/0
% Outputs the game's exit message
quit_menu :-
    write('\n\nThank you for playing. Goodbye!\n\n'),
    abort. 

% display_game(+GameState)
% Display the game's Board to the screen
display_game(GameState) :-
    draw_board(GameState).

% finish(+Player)
% Displays who won the game
finish((c-L)/_) :-
    L = 1 ->
    (
        write('Roger, roger!\n'),
        !, abort
    );
    (
        write('I\'m sorry Dave, I\'m afraid I can\'t let you do that...\nI win.\n'),
        !, abort
    ).

finish(h/Color) :-
    Color = 'd' -> 
    (
        write('Dark Pieces you won!!!\n'),
        !, abort
    );
    (
        write('Light Pieces you won!!!\n'),
        !, abort
    ).

% end_draw/0
% Lets the players know there's been a draw
end_draw :-
    write('Draw! Looks like you are evenly matched!\n').

% BOARD DRAWING %

% ┌────┬────┬────┬────┐
% │    │    │    │    │
% ┼────┼────┼────┼────┼
% │    │    │    │    │
% ┼────┼────┼────┼────┼
% │    │    │    │    │
% ┼────┼────┼────┼────┼
% │    │    │    │    │
% └────┴────┴────┴────┘

% draw_board(+Board)
% Displays the board
draw_board(Board) :-              
    write('    A   B   C   D  \n  _________________\n'),
    draw_lines(Board), nl,
    draw_stacks(Board).

% draw_lines(+Lines)
% Draws either:
% - the lines in the middle of the board
% - the bottom line if Lines is empty
draw_lines([]) :-
    write('  _________________'),nl.
             
draw_lines([X]) :-
    length(L, Length),
    Length1 is (Length - 4)*(-1),
    write(Length1),
    write(' | '),
    draw_cells(X),
    draw_lines(L).

draw_lines([X|L]) :-
    length(L, Length),
    Length1 is (Length - 4)*(-1),
    write(Length1),
    write(' | '),
    draw_cells(X),         
    write('  |---|---|---|---|\n'),
    draw_lines(L).

% draw_cells(+Cells)
% Draws the cells of each position of the board
draw_cells([]) :- nl,!.
draw_cells([X|L]) :-
    draw_cell(X),
    write(' | '),
    draw_cells(L).

% draw_cell(+Stack)
% Draws a cell, which is the element at the top of the given Stack
draw_cell(X) :-
    sub_atom(X, _, 1, 0, NX),
    write(NX).

% draw_stacks(+Stacks)
% Displays the stacks at each of the board's positions
draw_stacks([]) :- !.
draw_stacks([X|L]) :-
    length(L, Length),
    Length1 is (Length - 4)*(-1),
    draw_stack_line(X, Length1), nl,
    draw_stacks(L).

% Displays the stacks at the positions of each row of the board
draw_stack_line([], _) :- nl, !.
draw_stack_line([X|L], R) :-
    length(L, Length),
    C1 is 64+(Length - 4)*(-1),
    char_code(C, C1),
    draw_stack(X, R, C),
    write('\t'),
    draw_stack_line(L, R).

% draw_stack
% Outputs the stack at a given position
draw_stack(X, R, C) :-
    write(C),write(R),write(' = ['), write(X), write(']').

% Movement %

% get_by_index(+List, +Index, -Line)
% Retrieves the element of the List at the given Index
get_by_index([X|_], 0, X) :- !.
get_by_index([_|L], Index, Line) :-
    NewIndex is Index-1,
    get_by_index(L, NewIndex, Line).

% get_stack(+Board, +Position, -Stack)
% Retrieves the stack at a given Position from the Board
get_stack(Board, Col/Row, Stack) :-
    get_by_index(Board, Row, Line),
    get_by_index(Line, Col, Stack).

% get_stack_top(+Board, +Position, -Top)
% Retrieves the top piece of the stack at a given Position from the Board
get_stack_top(Board, Position, Top) :-
    get_stack(Board, Position, Stack),
    sub_atom(Stack, _, 1, 0, Top).

% row_col_to_value(+Col/Row, -X, -Y)
% Converts the user's Column/Row input into coordenates (e.g: a/1 -> 0/0)
row_col_to_value(Col/Row, X, Y) :-
    char_code(Col, ColCode),
    X is ColCode-97,
    Y is Row-1.

% row_col_to_value(+Col/Row, -X, -Y)
% Converts coordenates to user readable Column/Row values (e.g: 0/0 -> a/1)
value_to_row_col(Col/Row, X, Y) :-
    ColCode is Col+65,
    char_code(ColSymb, ColCode),
    X = ColSymb,
    Y is Row+1.

% valid_row_col(+Col/Row)
% Checks if the given Col/Row pair is valid
valid_row_col(Col/Row) :- 
    member(Row, [1, 2, 3, 4]),
    member(Col, ['a', 'b', 'c', 'd']).

% valid_piece_placement(+Board, +Pos)
% Checks whether or not the player is allowed to place a piece at the given Pos
valid_piece_placement(Board, Pos) :-
    get_stack(Board, Pos, Stack),
    \+ member(Stack, [' ']).

% get_piece_placement(+Board, -Pos)
% Asks the user where he wants to place his piece, parsing and validating his input
get_piece_placement(Board, Pos) :-
    write('Where would you like to place your piece (Col/Row)?\n'),
    read(Placement),
    (
        valid_row_col(Placement) ->
        (
            row_col_to_value(Placement, X, Y),
            Pos = X/Y,
            valid_piece_placement(Board, Pos) ->
            (
                !
            ); 
            write('Not a valid place! Try again...\n'),
            !, get_piece_placement(Board, Pos)
        );
        write('Not a valid place! Try again...\n'),
        !, get_piece_placement(Board, Pos)
    ).

% check_backtrack(+CurrPos, +NewPos)
% Checks whether or not the player is trying to move to his current position
check_backtrack(CX/CY, NX/NY) :-
    CY = NY, CX = NX.

% check_valid_move(+CurrPos, +NewPos)
% Checks if the given NewPos is valid
check_valid_move(CY/CX, NY/NX) :-
    (NX is CX + 1 ; NX is CX - 1),
    CY = NY.

check_valid_move(CY/CX, NY/NX) :-
    CX = NX,
    (NY is CY + 1 ; NY is CY - 1).

% get_move(+Board, +Stack, +CurrPos, +PrevPos, -Move)
% Asks the user where he would like to move his stack to
% - Reads the user's input
% - Verifies if it's valid and allowed
% - Parses it into a position and returns it
get_move(Board, Stack, CurrPos, PrevPos, Move) :- 
    value_to_row_col(CurrPos, CurrY, CurrX),
    value_to_row_col(PrevPos, PrevY, PrevX),
    format('Where would you like to move (Row/Col?)~nCurrent stack: ~w~nCurrent position: ~w~w~nPrevious position: ~w~w~n', [Stack, CurrY, CurrX, PrevY, PrevX]),
    read(Placement),
    (
        valid_row_col(Placement) ->
        (
            row_col_to_value(Placement, X, Y),
            Pos = X/Y,
            \+ check_backtrack(Pos, PrevPos) -> 
            (
                row_col_to_value(Placement, X, Y),
                Pos = X/Y, 
                check_valid_move(CurrPos, Pos) ->
                (
                    Move = Pos, !
                );
                write('Invalid move! Try again...\n'),
                !, get_move(Board, Stack, CurrPos, PrevPos, Move)
            );
            write('Invalid move! Try again...\n'),
            !, get_move(Board, Stack, CurrPos, PrevPos, Move)
        );
        write('Invalid move! Try again...\n'),
        !, get_move(Board, Stack, CurrPos, PrevPos, Move)
    ).

