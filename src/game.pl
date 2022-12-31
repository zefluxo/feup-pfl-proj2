:-consult('io.pl').
% BOARD/MODE %

tan('t').
dark('d').
light('l').
board([['tt', ' ', ' ', 'tt'],
       [' ', ' ', ' ', ' '],
       [' ', ' ', ' ', ' '],
       ['tt', ' ', ' ', 'tt']]).

mode(h/h).
:- dynamic(mode/1).

get_by_index([X|_], 0, X) :- !.
get_by_index([X|L], Index, Item) :-
    NewIndex is Index-1,
    get_by_index(L, NewIndex, Item).

get_stack(Board, Row/Col, Stack) :-
    get_by_index(Board, Row, Line),
    get_by_index(Line, Col, Stack).

get_stack_top(Board, Placement, Top) :-
    get_stack(Board, Placement, Stack),
    sub_atom(Stack, _, 1, 0, Top).

replace([_|T], 0, X, [X|T]):- !.
replace([H|T], I, X, [H|R]):-
    I > 0,
    I1 is I-1,
    replace(T, I1, X, R).

stack_split(Stack, Index, First, Second) :-
    atom_codes(Stack, Codes),
    length(FirstCodes, Index),
    append(FirstCodes, SecondCodes, Codes),
    atom_codes(First, FirstCodes),
    atom_codes(Second, SecondCodes).

% MENU LOGIC %

play :-
    menu(main_menu).

menu(main_menu) :- 
    main_menu,
    read(Opt),
    (
        Opt =:= 1 ->
            menu(play),
            start_game;
        Opt =:= 2 -> 
            exit_msg,
            fail;
        error_msg,
        menu(main_menu)
    ).

menu(quit) :- quit_menu.
menu(play) :- play_menu.

% GAME LOGIC %

start_game :-
    initial_state(Board),
    display_game(Board),    
    mode(StartPlayer/SndPlayer),
    run_game(Board-16, StartPlayer-d, SndPlayer-l).

initial_state(Board) :-
    board(Board).

run_game(Board-0, Player, SndPlayer) :-    
    game_over(Board, Winner), !,
    finish(Winner).
run_game(Board-Turns, Player, SndPlayer) :-
    (

        get_piece_placement(Board, Pos), % Gets us the position of the stack to be played
        place_piece(Board, Player, Pos, PBoard), % Places the piece effectively on the right stack on the board
        display_game(PBoard),
        NewTurns is Turns - 1,
        CurrPos = Pos,
        PrevPos = Pos,
        get_stack(PBoard, CurrPos, Stack),
        remove_stack(PBoard, CurrPos, RBoard),
        move_stack(RBoard, CurrPos, PrevPos, Stack, MBoard),
        write('\n\n NEW PLAYER TURN SWITCH AROUND NOW \n\n'),
        display_game(MBoard),
        run_game(MBoard-NewTurns, SndPlayer, Player)

    ).

move_stack(Board, CurrPos, PrevPos, Stack, NBoard) :-
    atom_length(Stack, Length),
    Length > 0 -> 
    (
        get_move(Board, Stack, CurrPos, PrevPos, Move),
        NPrevPos = CurrPos,
        move(Board, Stack, Move, NStack, RBoard),
        display_game(RBoard),
        move_stack(RBoard, Move, NPrevPos, NStack, NBoard)
    ); NBoard = Board, !.

remove_stack(Board, X/Y, NBoard) :-
    get_by_index(Board, Y, Line),
    replace(Line, X, ' ', NLine),
    replace(Board, Y, NLine, NBoard).


game_over(GameState, Winner) :-
    % CHECK IF GAME OVER
    write('Not Working Yet'), fail.

place_piece(Board, Player-Color, X/Y, PBoard) :-
    get_by_index(Board, Y, Line),
    get_by_index(Line, X, Stack),
    atom_concat(Stack, Color, NStack),
    replace(Line, X, NStack, NLine),
    replace(Board, Y, NLine, PBoard).

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

