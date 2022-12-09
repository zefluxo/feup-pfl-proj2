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

curr_pieces(8).
adv_pieces(8).


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
    initial_state(GameState),
    display_game(GameState),
    mode(StartPlayer/_),
    run_game(GameState/StartPlayer).


initial_state(Board-dark) :-
    board(Board).

run_game(GameState/Player) :-    
    game_over(GameState, Winner), !,
    finish(Winner).
run_game(GameState/Player) :-
    (
        check_piece_amount ->
        get_piece_placement(GameState, Player, PGameState),
        place_piece(PGameState, Player, NPGameState)
    ),
    get_move(GameState, Player, Move),
    move(GameState, Player, NGameState),
    switch_player(Player, NPlayer),
    display_game(NGameState), !,
    run_game(NGameState/NPlayer).

game_over(GameState, Winner) :-
    % CHECK IF GAME OVER
    % LAST ELEMENT OF 4 STACKS IN A ROW (any direction v/h/d) IS THE SAME
    write('Not Working Yet'), fail.

check_piece_amount :-
    curr_pieces(X),
    X > 0.

place_piece(PGameState, Player, NPGameState). 
move(GameState, Player, NGameState).
switch_player(Player, NPlayer).
