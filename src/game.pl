:-consult('io.pl').

% BOARD/MODE %

tan('t').
dark('d').
light('l').
board([['tt', ' ', ' ', 'tt'],
       [' ', ' ', ' ', ' '],
       [' ', ' ', ' ', ' '],
       ['tt', ' ', ' ', 'tt']]).

:- dynamic(mode/1).
mode(h/h).

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
    game_over(Board, Player),
    game_over(Board, SndPlayer).

run_game(Board-Turns, Player, SndPlayer) :-
    not(game_over(Board, Player)) -> 
    (
        get_piece_placement(Board, Pos), % Gets us the position of the stack to be played
        place_piece(Board, Player, Pos, PBoard), % Places the piece effectively on the right stack on the board
        display_game(PBoard),
        NewTurns is Turns - 1,
        CurrPos = Pos,
        PrevPos = Pos,
        get_stack(PBoard, CurrPos, Stack),
        remove_stack(PBoard, CurrPos, RBoard),
        move_stack(Player, RBoard, CurrPos, PrevPos, Stack, MBoard),
        run_game(MBoard-NewTurns, SndPlayer, Player)

    ); 
    finish(Player), !.

move_stack(Player, Board, CurrPos, PrevPos, Stack, NBoard) :-
    atom_length(Stack, Length),
    Length > 0 -> 
    (
        not(game_over(Board, Player)) -> (
            get_move(Board, Stack, CurrPos, PrevPos, Move),
            NPrevPos = CurrPos,
            move(Board, Stack, Move, NStack, RBoard),
            display_game(RBoard),
            move_stack(Player, RBoard, Move, NPrevPos, NStack, NBoard)
        ); finish(Player), !

    ); 
    game_over(Board, Player) -> finish(Player), !;
    write('Time for the other player to have his turn!\n\n'),
    NBoard = Board, !.

remove_stack(Board, X/Y, NBoard) :- 
    get_by_index(Board, Y, Line),
    replace(Line, X, ' ', NLine),
    replace(Board, Y, NLine, NBoard).


game_over(Board, Player) :-
    % CHECK IF GAME OVER
    check_horizontals(Board, Player, 0);
    check_verticals(Board, Player, 0);
    check_diagonals(Board, Player).

check_horizontal(Row, Player-Color) :-
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

check_horizontals(Board, Player-Color, RowNumber) :-
    RowNumber < 4 ->
    (
        get_by_index(Board, RowNumber, Row),
        check_horizontal(Row, Player-Color) -> 
        (
            true
        );
        NRowNumber is RowNumber + 1,
        check_horizontals(Board, Player-Color, NRowNumber)

    ).

check_verticals(Board, Player-Color, ColNumber) :-
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
        check_verticals(Board, Player-Color, NColNumber)

    ).

check_first_diagonal(Board, Player-Color) :-
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

check_second_diagonal(Board, Player-Color) :-
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

check_diagonals(Board, Player-Color) :-
    check_first_diagonal(Board, Player-Color);
    check_second_diagonal(Board, Player-Color).

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

