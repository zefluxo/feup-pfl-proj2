:-consult('io.pl').

% BOARD/MODE %

% board(+Board)
% Function to define the game's board
board([['tt', ' ', ' ', 'tt'],
       [' ', ' ', ' ', ' '],
       [' ', ' ', ' ', ' '],
       ['tt', ' ', ' ', 'tt']]).

% Declare the game_mode as dynamic and default it to (h/h)
:- dynamic game_mode/1.
game_mode(h/h).

% replace(+List, +Index, +Replacement, -NewList)
% Replaces the element of the List at a given Index with Replacement
% returning the result in NewList
replace([_|T], 0, X, [X|T]):- !.
replace([H|T], I, X, [H|R]):-
    I > 0,
    I1 is I-1,
    replace(T, I1, X, R).

% stack_split(+Stack, +Index, -First, -Second)
% Splits the elements of a stack at a given index
% and stores the left half in First and the right half in Second
stack_split(Stack, Index, First, Second) :-
    atom_codes(Stack, Codes),
    length(FirstCodes, Index),
    append(FirstCodes, SecondCodes, Codes),
    atom_codes(First, FirstCodes),
    atom_codes(Second, SecondCodes).

% MENU LOGIC %

% play/0
% Starts the program
play :-
    menu(main_menu).

% menu(+Menu)
% Starts by drawing the given Menu and 
% parses input to decide what to do next
menu(main_menu) :- 
    main_menu,
    read(Opt),
    (
        Opt =:= 1 ->
            menu(play),
            start_game;
        Opt =:= 2 -> 
            menu(quit),
        error_msg,!,
        menu(main_menu)
    ).

menu(quit) :- quit_menu.
menu(play) :- 
    play_menu(Mode),
    retractall(game_mode(_)),
    assert(game_mode(Mode)), !.

% wait_for_input/0
% Waits for an input from user
wait_for_input :-
    write('Write anything to continue (p.e: [c.])!\n'),
    read(_).

% clear_screen/0
% Clears the terminal
clear_screen :-
    write('\33\[2J').

% GAME LOGIC %

% start_game/0
% Responsible for starting the game
% - Sets the initial board
% - Sets the game_mode
% - Runs the game
start_game :-
    initial_state(Board),
    game_mode(StartPlayer/SndPlayer),
    run_game(Board-16, StartPlayer/d, SndPlayer/l).

% initial_state(+Board)
% Initializes the game's board
initial_state(Board) :-
    board(Board).

% run_game(+Board-Turns, +Player/Color, +SndPlayer)
% Main game loop that decides the result of the game when there are no more turns
% - Displays the current board
% - Asks the current player to place one of his pieces, verifying whether it's being placed somewhere that's allowed
% - Updates the turns left and the positions to make sure there are no illegal moves
% - Removes the stack where the player put his piece on from the board
% - Moves the stack to adjacent locations input by the player, leaving behind the piece at the bottom
% - Repeat until the stack is empty
% - Checks for a game over
% - Passes the turn to the other player
run_game(_-0, _, _) :- end_draw, abort, !.
run_game(Board-_, Player, SndPlayer) :-
    game_over(Board, SndPlayer) -> 
    (
        finish(SndPlayer)
    );
    game_over(Board, Player) ->
    (
        finish(Player)
    ).

run_game(Board-Turns, h/Color, SndPlayer) :-
    (
        clear_screen,
        write(Color), write(' is now playing:\n'),
        display_game(Board),
        get_piece_placement(Board, Pos), % Gets us the position of the stack to be played
        place_piece(Board, h/Color, Pos, PBoard), % Places the piece effectively on the right stack on the board
        display_game(PBoard),
        NewTurns is Turns - 1,
        CurrPos = Pos,
        PrevPos = Pos,
        get_stack(PBoard, CurrPos, Stack),
        remove_stack(PBoard, CurrPos, RBoard),
        move_stack(Player, RBoard, CurrPos, PrevPos, Stack, MBoard),
        run_game(MBoard-NewTurns, SndPlayer, h/Color)

    ).

run_game(Board-Turns, (c-Level)/Color, SndPlayer) :-
    clear_screen,
    write(Color), write(' is now playing:\n'),
    display_game(Board),
    cpu_placement(Board, (c-Level)/Color, Pos),
    place_piece(Board, (c-Level)/Color, Pos, PBoard),
    write('This is the board after placing a piece: \n'),
    display_game(PBoard),
    NewTurns is Turns - 1,
    CurrPos = Pos,
    PrevPos = Pos,
    get_stack(PBoard, CurrPos, Stack),
    remove_stack(PBoard, CurrPos, RBoard),
    write('Movement phase: \n'),
    cpu_movement(Level, Color, RBoard, CurrPos, PrevPos, Stack, MBoard),
    wait_for_input,
    run_game(MBoard-NewTurns, SndPlayer, (c-Level)/Color).

% cpu_movement(+Level, +Color, +Board, +CurrPos, +PrevPos, +Stack, -NBoard)
% The computer's movement function
% - Checks the Level to decide on which algorithm to use
% - If the Level is 1, it chooses random moves from the list of possible moves
% - If the Level is 2, it chooses moves prioritizing spaces with enemy pieces at the bottom
cpu_movement(Level, Color, Board, CurrPos, PrevPos, Stack, NBoard) :-
    (
        Level is 1 ->
        (
            atom_length(Stack, Length),
            Length > 0 -> 
            (
                find_moves(Board, CurrPos, PrevPos, Moves),
                random_member(Move, Moves),
                NPrevPos = CurrPos,
                move(Board, Stack, Move, NStack, MBoard),
                display_game(MBoard),
                cpu_movement(Level, Color, MBoard, Move, NPrevPos, NStack, NBoard)
            ); NBoard = Board, !
        
        );
        Level is 2 ->
        (
            atom_length(Stack, Length),
            Length > 0 -> 
            (
                find_moves(Board, CurrPos, PrevPos, Moves),
                nth0(0, Moves, Best),
                stack_split(Stack, 1, Bottom, NStack),
                (Bottom = Color ->
                (
                    Value is 0
                );
                (
                    Value is 100
                )),
                value_move(Board, Bottom, Color, Moves, Best-Value, Move),
                NPrevPos = CurrPos,
                move(Board, Stack, Move, NStack, MBoard),
                display_game(MBoard),
                cpu_movement(Level, Color, MBoard, Move, NPrevPos, NStack, NBoard)
            ); NBoard = Board, !
        )
    ).

% value_move(+Board, +Bottom, +Color, +Moves, ?Best-Value, +Move)
% Chooses the best move amongst the list of possible moves
% - Chooses a position from the list of possible moves
% - Calculates the position's value
% - Checks if the piece at the bottom of the stack in the current position is an enemy piece
% - If so and NValue is greater than the current Value, it updates the Best-Value to (CurrentPosition-NValue)
% - Else, if NValue is lesser than the current Value, it updates the Best-Value to (CurrentPosition-NValue)
% - Repeats itself using the next position from the Moves list until it's empty, returning Best as Move
value_move(Board, Bottom, Color, [], Best-Value, Best).
value_move(Board, Bottom, Color, [CurrPos|L], Best-Value, Move) :-
    (
        Bottom = Color ->
        (
            row_value(Board, Color, CurrPos, RowValue),
            col_value(Board, Color, CurrPos, ColumnValue),
            NValue is RowValue + ColumnValue,
            (
                NValue > Value ->
                value_move(Board, Bottom, Color, L, CurrPos-NValue, Move)
                ;value_move(Board, Bottom, Color, L, Best-Value, Move)
            )
        );
        (
            row_value(Board, Color, CurrPos, RowValue),
            col_value(Board, Color, CurrPos, ColumnValue),
            NValue is RowValue + ColumnValue,
            (
                NValue < Value ->
                value_move(Board, Bottom, Color, L, CurrPos-NValue, Move)
                ;value_move(Board, Bottom, Color, L, Best-Value, Move)
            )
        )
           
    ).

% row_value(+Board, +Color, +Pos, -NValue)
% Calculates how valuable a move in the current row would be
row_value(Board, Color, (X/_), NValue) :-
    nth0(X, Board, Row),
    line_count_color(Row, Color, 0, NValue).


% col_value(+Board, +Color, +Pos, -NValue)
% Calculates how valuable a move in the current column would be
col_value(Board, Color, (_/Y), NValue) :-
    transpose(Board, NBoard),
    nth0(Y, NBoard, Col),
    line_count_color(Col, Color, 0, NValue).

% line_count_color(+Line, +Color, ?Inc, -Value)
% Adds up the value of the stacks in a given line relative to the current CPU's color and enemy's color
line_count_color([], Color, Inc, Value) :- Value is Inc.
line_count_color([Stack|L], Color, Inc, Value) :-
    atom_chars(Stack, StackChars),
    stack_count_color(StackChars, Color, 0, StackValue),
    NInc is StackValue + Inc,
    line_count_color(L, Color, NInc, Value).

% stack_count_color(+Stack, +Color, ?Inc, -Count)
% Counts the number of pieces of the given Color in the Stack
stack_count_color([], _, Inc, Count) :- Count is Inc.
stack_count_color([X|L], Color, Inc, Count) :-
    (
        X = Color ->
        NInc is Inc + 1
        ;NInc is Inc
    ),
    stack_count_color(L, Color, NInc, Count).

% cpu_placement(+Board, +(Computer-Level)/Color, -Place)
% Chooses the best position to place a piece at
% - Checks the Level to decide on which algorithm to use
% - If the Level is 1, it chooses a random possible position to place the piece at 
% - If the Level is 2, it chooses a position based on the amount of enemy pieces in it
cpu_placement(Board, (c-Level)/Color, Place) :-
    (
        Level is 1 ->
        (
            find_placements(Board, Places),
            random_member(Place, Places)
        );
        Level is 2 ->
        (
            find_placements(Board, Places),
            nth0(0, Places, Best),
            Value is 0,
            value_place(Board, Color, Places, Best-Value, Place)
        )
    ).

% value_place(+Board, +Color, +Places, ?Best-Value, -Place)
% Calculates the best position to place a piece at from the list of possible locations
% - Chooses a position
% - Checks the number of enemy pieces on the current position's stack
% - If this number is larger than the current Best-Value, it gets updated to
% (CurrentPosition-NewValue)
% - Moves to the next position until there are no places left to check and returns the best location
value_place(Board, Color, [], Best-Value, Best).
value_place(Board, Color, [CurrPos|L], Best-Value, NBest) :-
    get_stack(Board, CurrPos, Stack),
    atom_chars(Stack, StackChars),
    stack_count_color(StackChars, Color, 0, StackValue),
    (
        StackValue > Value ->
        value_place(Board, Color, L, CurrPos-StackValue, NBest)
        ;value_place(Board, Color, L, Best-Value, NBest)
    ).

% within_board(+Board, +Position)
% Checks if a given position isn't out of bounds
within_board(_, X/Y) :-
    between(0, 3, X), 
    between(0, 3, Y).

% backtracking(+PrevPos, +CurrPos)
% Checks if the player isn't trying to move back to the previous spot
backtracking(PX/PY, X/Y) :-
    X = PX, Y = PY.

% valid_placement(+Board, +Pos)
% Checks if a piece can be placed at the given position
valid_placement(Board, X/Y) :-
    within_board(Board, X/Y),
    nth0(Y, Board, Row),
    nth0(X, Row, Placement),
    Placement \= ' '.

% find_placements(+Board, -Places)
% Retrieves all the positions where placing a piece is possible
find_placements(Board, Places) :-
    findall(Move, valid_placement(Board, Move), Places).

% valid_movement(+Board, +CurrPos, +PrevPos, +NewPos)
% Checks if moving to the NewPos is valid
valid_movement(Board, CX/CY, PX/PY, NX/NY) :-
    % inside the board %
    within_board(Board, NX/NY),
    % not backtracking %
    \+ backtracking(PX/PY, NX/NY),
    % orthogonally adjacent %
    (((NX is CX - 1; (NX is CX+1, CX >= 0)), CY = NY);
    ((NY is CY - 1; (NY is CY+1, CY >= 0)), CX = NX)).

% find_moves(+Board, +CurrPos, +PrevPos, -Moves)
% Retrieves all the positions where it is possible to move to  
find_moves(Board, CurrPos, PrevPos, Moves) :-
    findall(Move, valid_movement(Board, CurrPos, PrevPos, Move), Moves).

% move_stack(+Player, +Board, +CurrPos, +PrevPos, +Stack, -NBoard)
% Moves the player's stack
% While the stack's length is greater than 0, it:
% - Asks the player for a position to move to
% - Updates positions to avoid illegal moves
% - Moves the stack to the given position, leaving the piece at its bottom behind
% - Updates the board and stores it in NBoard
% - Repeats
% When the stack's length is 0, it returns the board after movement
move_stack(Player, Board, CurrPos, PrevPos, Stack, NBoard) :-
    atom_length(Stack, Length),
    Length > 0 -> 
    (
        get_move(Board, Stack, CurrPos, PrevPos, Move),
        NPrevPos = CurrPos,
        move(Board, Stack, Move, NStack, RBoard),
        display_game(RBoard),
        move_stack(Player, RBoard, Move, NPrevPos, NStack, NBoard)

    ); 
    NBoard = Board, !.

% remove_stack(+Board, +Pos, -NBoard)
% Removes a stack from the board at a given position
remove_stack(Board, X/Y, NBoard) :- 
    get_by_index(Board, Y, Line),
    replace(Line, X, ' ', NLine),
    replace(Board, Y, NLine, NBoard).


% game_over(+Board, +Player)
% Checks if the Player has won the game
game_over(Board, Player) :-
    % CHECK IF GAME OVER
    check_horizontals(Board, Player, 0);
    check_verticals(Board, Player, 0);
    check_diagonals(Board, Player).

% check_horizontal(+Row, +Player/Color)
% Checks if all the stacks in the given Row have a piece of the given Color at the top
check_horizontal(Row, _/Color) :-
    get_by_index(Row, 0, Stack1), 
    get_by_index(Row, 1, Stack2),
    get_by_index(Row, 2, Stack3),
    get_by_index(Row, 3, Stack4),
    sub_atom(Stack1, _, 1, 0, Element1),
    sub_atom(Stack2, _, 1, 0, Element2),
    sub_atom(Stack3, _, 1, 0, Element3),
    sub_atom(Stack4, _, 1, 0, Element4),

    Element1 = Color ->
    (
        (Element1 = Element2, Element2 = Element3, Element3 = Element4)
    ).

% check_horizontals(+Board, +Player/Color, ?RowNumber)
% Checks all rows of the board for a win
check_horizontals(Board, Player/Color, RowNumber) :-
    RowNumber < 4 ->
    (
        get_by_index(Board, RowNumber, Row),
        check_horizontal(Row, Player/Color) -> 
        (
            true
        );
        NRowNumber is RowNumber + 1,
        check_horizontals(Board, Player/Color, NRowNumber)

    ).

% check_verticals(+Board, +Player/Color, ?ColNumber)
% Checks if the stacks in each column have a piece of the given Color at the top
check_verticals(Board, Player/Color, ColNumber) :-
    ColNumber < 4 ->
    (
        get_by_index(Board, 0, Line1),
        get_by_index(Board, 1, Line2),
        get_by_index(Board, 2, Line3),
        get_by_index(Board, 3, Line4),
    
        get_by_index(Line1, ColNumber, Stack1),
        get_by_index(Line2, ColNumber, Stack2),
        get_by_index(Line3, ColNumber, Stack3),
        get_by_index(Line4, ColNumber, Stack4),

        sub_atom(Stack1, _, 1, 0, Element1),
        sub_atom(Stack2, _, 1, 0, Element2),
        sub_atom(Stack3, _, 1, 0, Element3),
        sub_atom(Stack4, _, 1, 0, Element4),

        Element1 = Color ->
        (
            Element1 = Element2, Element2 = Element3, Element3 = Element4
        );
        NColNumber is ColNumber + 1,
        check_verticals(Board, Player/Color, NColNumber)

    ).

% check_first_diagonal(+Board, +Player/Color)
% Checks if all the stacks in the 1st diagonal have a piece of the given Color at the top
check_first_diagonal(Board, _/Color) :-
        get_by_index(Board, 0, Line1),
        get_by_index(Board, 1, Line2),
        get_by_index(Board, 2, Line3),
        get_by_index(Board, 3, Line4),

        get_by_index(Line1, 0, Stack1),
        get_by_index(Line2, 1, Stack2),
        get_by_index(Line3, 2, Stack3),
        get_by_index(Line4, 3, Stack4),

        sub_atom(Stack1, _, 1, 0, Element1),
        sub_atom(Stack2, _, 1, 0, Element2),
        sub_atom(Stack3, _, 1, 0, Element3),
        sub_atom(Stack4, _, 1, 0, Element4),

        Element1 = Color ->
        (
            Element1 = Element2, Element2 = Element3, Element3 = Element4
        ).


% check_second_diagonal(+Board, +Player/Color)
% Checks if all the stacks in the 2nd diagonal have a piece of the given Color at the top
check_second_diagonal(Board, Player/Color) :-
        get_by_index(Board, 0, Line1),
        get_by_index(Board, 1, Line2),
        get_by_index(Board, 2, Line3),
        get_by_index(Board, 3, Line4),

        get_by_index(Line1, 3, Stack1),
        get_by_index(Line2, 2, Stack2),
        get_by_index(Line3, 1, Stack3),
        get_by_index(Line4, 0, Stack4),

        sub_atom(Stack1, _, 1, 0, Element1),
        sub_atom(Stack2, _, 1, 0, Element2),
        sub_atom(Stack3, _, 1, 0, Element3),
        sub_atom(Stack4, _, 1, 0, Element4),

        Element1 = Color ->
        (
            Element1 = Element2, Element2 = Element3, Element3 = Element4
        ).


% check_diagonals(+Board, +Player/Color)
% Checks both diagonals for a win
check_diagonals(Board, Player/Color) :-
    check_first_diagonal(Board, Player/Color);
    check_second_diagonal(Board, Player/Color).

% place_piece(+Board, +Player/Color, +Pos, -PBoard)
% Places a piece at the top of the stack in the given Pos
place_piece(Board, _/Color, X/Y, PBoard) :-
    get_by_index(Board, Y, Line),
    get_by_index(Line, X, Stack),
    atom_concat(Stack, Color, NStack),
    replace(Line, X, NStack, NLine),
    replace(Board, Y, NLine, PBoard).

% move(+Board, +Stack, +X/Y, -NStack, -NBoard)
% Moves the given Stack, leaving the piece at its bottom in the given Pos
move(Board, Stack, X/Y, NStack, NBoard) :-
    get_stack(Board, X/Y, Replacing), % In Board Location Stack
    (
        Replacing = ' ' ->
        NReplacing = '';
        NReplacing = Replacing
    ),
    stack_split(Stack, 1, Bottom, NStack), % Bottom is the thing to drop into Replacing and Top is the new stack (moving)
    atom_concat(NReplacing, Bottom, Replaced),
    get_by_index(Board, Y, Line),
    replace(Line, X, Replaced, NLine),
    replace(Board, Y, NLine, NBoard).
