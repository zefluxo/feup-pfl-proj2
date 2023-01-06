# pfl-qawale

### Installation

Start off by installing either SWIProlog or SicStus Prolog compiler, making sure that you clone the main branch if you intend to use SWIProlog or the sicstus branch for SicStus. You can start the program by running `consult('main.pl')` while in the `/src` folder and inputting `play.`

### Project Description

This project was made as part of the PFL unit of the L.EIC course, with the goal being to implement a two-player board game in Prolog, targetting the SICStus Prolog version 4.7.1 compiler. The game should be correctly implemented according to its rules, and presenting the correct current state of the board and the pieces. Furthermore there should be three types of players, Human and 2 levels of CPUs, with 3 modes of use: Human/Human, Human/Computer and Computer/Computer.

The group, Qawale_4, chose the game Qawale to be implemented and is comprised by António Oliveira Santos (up202008004) and José Luís Nunes Osório (up202004653). The workload was evenly distributed, meaning each member contributed 50% effort to its completion.

### Game Descritpion

[Qawale](https://www.hachetteboardgames.com/products/qawale) is a simple yet tactical board game, where two players with 8 colored pieces of their own, take turns to place a piece on top of a pre-existing stack and then move the same stack leaving a piece on their path. There's also a third neutral colored set of 8 pieces that are placed in the beggining of the game on each of the 4 corners of the 4x4 board, in stacks of 2.

[The rules of the game](https://randolphca.sharepoint.com/sites/Public/Documents%20partages/Forms/AllItems.aspx?id=%2Fsites%2FPublic%2FDocuments%20partages%2FSales%20%2D%20Ventes%2FTOOLS%20%2D%20OUTILS%2FVisuels%20jeux%20%2D%20Games%20Visual%2FUSA%2FQawale%20%2D%20media%20kit%2FQawale%20%2D%20rules%2Epdf&parent=%2Fsites%2FPublic%2FDocuments%20partages%2FSales%20%2D%20Ventes%2FTOOLS%20%2D%20OUTILS%2FVisuels%20jeux%20%2D%20Games%20Visual%2FUSA%2FQawale%20%2D%20media%20kit&p=true&ga=1) are simple, yet allow the game to have some depth. At the start of the game, after setting up the board with the neutral stacks on its corners, players place a piece on top of a stack, pieces can NOT be placed on empty spaces but may be placed on top of whatever stack players choose. The player then moves the stack along the board in the orthogonal directions, dropping the bottom stone of the stack at an adjacent place to the previous one, and being able to switch directions at will but unable to backtrack to the immediately before visited place. After moving the stack fully, players switch and the cycle repeats, until one of the end conditions is met.
The goal of the game is to create a visibile "4 in a row" line on the top layer of the stacks, that is, from an "aerial view", 4 pieces in a row, vertically, horizontally or in any of the 2 diagonals. A vertical stack of 4 same colored pieces does not present a valid winning condition. If both players placed all of their pieces without any of them meeting the previous win condition the game is terminated and is concidered a draw.

### Game Logic

#### Internal representation

**Board:** The game is played in a 4x4 board, which contains stacks of pieces. As such the use of a list of lists, or a 2D matrix came to mind when designing its representation and is being currently used for this purpose. The initial board can be found represented in the ```board\1``` predicate. 

**Stacks:** Stacks are the one of the main elements of the game, as such their representation came with special consideration, since they are some the most manipulated element in the game. After debating on this topic the group decided to use atoms for their representation, as we would be treating them similar to strings in a more conventional programming language. It is also important to mention that stacks are read from left to right, meaning that the leftmost "character" is the bottom and the rightmost the top.

**Pieces:** Pieces are simply atoms as this simplifies the operations that must be done with them when interacting and comparing them with other pieces in stacks. In order to simplify identification of pieces, there are 3 atoms used, ```d``` for dark pieces, ```l``` for light pieces and ```t``` for tan pieces, the last ones are neutral to both players; this representation also pays homage to the game's original color scheme.

**Turns:** Due to Qawale's rigid turn limit, we decided to "hardcode" the turns, they will always be, in the worst case scenario 16, 8 pieces for each player that must be placed.

**Players:** Players are represented by both their type, ```h``` (human) or ```c-1```/```c-2``` (computer-level), and the color of their pieces, ```d``` or ```l```.

#### Game state view

The game would not be playable without correctly presenting the present state of the board to the players,
as such the predicate ```display_game(+Board)\1```, which receives the current state of the board and prints it in a user friendly readable manner.

This task was a pretty big challenge since Qawale is effectively a 3D game, played in a 2D board/matrix with the third dimension being the contents of each stack, and we are working with Sictus' rudimentar interactive terminal. A good representation of the board required some creativity, as simply displaying the top most layer of the board would hide important strategic information and only display the stacks would present itself confusing and frustating to figure out "who's winning". The idea that came to mind was to represent each stack inside the board, however this resulted in weird, misaligned and too information dense boards that were even harder to grasp, pushing us in the direction to fuse both initial methods. The final implementation features 2 displays, the top one being a 4x4 grid of the top layer of the board and the bottom one a representation of each stack aligned by columns and rows, ```COLROW = [<Stack>]```, labeled from A to B and 1 to 4 respectively.

Absent from this project is the possibility to experiment with different board sizes, this comes due to Qawale's strategic gameplay and well defined goal within its board. Smaller or larger boards would disrupt game balance and also make for longer and less intense games, furthermore there is no indication given by the creators of the game for this purpose.

#### Turns & Game Execution

The game is started by the predicate ```start_game\0```, which uses ```initial_state\1``` to fetch the starting board and ```game_mode\1``` to read the information regarding the type of players playing. All of this information is then passed onto ```run_game\3``` where the main loop of displaying, placing and moving takes place. At the start of each turn the game over conditions are verified and the ```run_game(_-0, _, _)``` definition of the ```run_game``` predicate ensures that games end when there aren't any more turns left/pieces to be played by each player.
Switching players is simply handled by switching around the last 2 arguemnts of the ```run_game(+Board-Turns, +Player/Color, +SndPlayer)``` predicate, as well as counting down ```Turns```.

##### Human Placement & Movement 

###### Placement

The first part of any turn is the placement phase, where a player chooses a pre-existing stack to place one of their colored pieces on top of. In this phase, when dealing with player input we must take into consideration the representation of the board previously described, with rows and columns labelled with numbers and letters, and be able to take an input in that format and convert it into a more usable representation of a position on the board. As such, input handling and validation is entirely handled within the predicate ```get_piece_placement(+Board, -Pos)``` and is aided by the predicate ```valid_row_col(+Col/Row)```, which, due to the fact that the only board size that exists is a 4x4, of which we know exactly all valid values of each coordinate simply must check if the input matches any of them.
After validating the input we must check if the given values actually represent a valid placement of a piece according to the previously stipulated rules. ```valid_piece_placement(+Board, +Pos)``` is a predicate that takes in the current board state and a numerical internal position and returns true when ```Pos``` corresponds to a valid position, that is, the stack at ```Pos``` is not empty; this predicate fills the role of a ```valid_moves```-like predicate for human piece placement. 

Following the rule-checking and validation of the input, it is returned to the ```run_game\3``` game loop where it is passed down to the predicate ```place_piece(Board, _/Color, X/Y, PBoard)``` where it is placed on top of the selected stack; this predicate would be equivalent to a ```move``` predicate for piece placement, as it effectively places the piece in the desired place. The new board is then displayed and the turn continues onto the movement phase.

###### Movement

Right after the end of the previous step, and before the start of this phase, the previous and current positions are updated and the stack chosen to have a piece placed on top of is removed from the board, as it would now, in real life, be in the hands of the player and not part of the board state. The player's movement phase is initiated by the ```move_stack(+Player, +Board, +CurrPos, +PrevPos, +Stack, -NBoard)``` predicate, where, whilst there are pieces in ```Stack```, the stack that is being moved by the player, the game prompts the player for a movement input, places a piece on the spot that was chosen, displays the new game state and repeats the process.
Input acquisition in this phase is done through the predicate ```get_move(+Board, +Stack, +CurrPos, +PrevPos, -Move)```, where, in a similar fashion to the one previously described in the placement phase, the input is checked to be in bounds and the game rules regarding movement are enforced, it is at this time that the input is checked to not be a backtrack and to be done onto an orthogonally adjacent position to the current one.

Once a valid move is inputed by the player the game then updates the previous position and proceeds onto the ```move(+Board, +Stack, +X/Y, +NStack, -NBoard)``` predicate, where the bottom-most piece of ```Stack``` is appended to the top of the stack on the board at ```(X,Y)```, finally a new board is created by replacing the stack on the board row at Y and then replacing said row in the board.

##### Computer Placement & Movement

Dealing with computer inputs is much easier, since we can accurately control and delimit their behaviour by virtue of their nature. The computer game loop closely resembles the human one, with its only major diference being the ```get_<placement/move>``` predicates, as such we will be discussing the implementations to replace these functionalities in greater detail in the upcoming sections, as well as the inner workings of each difficulty level.

###### Placement

Just like with humans, the first phase of a computer's turn is the selection of the piece placement, this is made possible by the ```cpu_placement(+Board, +(c-Level)/Color, -Place)``` predicate where by analyzing the list of possible valid movements, and taking into consideration the level and the strategy associated with each, a move is chosemn and returned in ```Place```.

In both levels the first step is to find all of the valid placements, made possible with ```find_placements(+Board, -Places)```, where using the ```findall\3``` predicate, along side the ```valid_placement(+Board, +X/Y)``` predicate, we are able to find all of the possible valid placements on the board. As previously described, a valid placement is one done in a non empty space, so this is exactly what the predicate ```valid_placement\2``` solves.

For a level 1 CPU, after finding all of the possible placements in the current board, the strategy is to simply take a random member from the list returned by ```find_placements\2```.
However in a level 2 CPU the strategy is more interesting. In this CPU level, after acquiring the list of possible valid moves, the predicate ```value_place(+Board, +Color, +[CurrPos|L], +Best-Value, -NBest)``` is executed in order to find the place with the most highly valued position on the board. A position's value is calculated by counting the number of ```Color```, the CPU's color, colored pieces in each stack, where a stack with more pieces of this color is more valuable. In essence, placing a piece on a stack with many pieces of the current player's color is highly valuable, as this gives a lot of control in the movement phase to the player of how their pieces are distributed on the board. This strategy comes with a significant draw back, being that it does not take into account the possibility of being 1 move away from a win but prioritizing the most amount of pieces in detriment of the win.

Eitherway, whatever level the computer is playing, once ```cpu_placement(+Board, +(c-Level)/Color, -Place)``` finishes it returns the best move it found according to each strategy and the game loop proceeeds just like described for the human player.

###### Movement

The movement phase in CPUs works similarly to the one already described for humans, as there is once more no real difference apart from acquiring the desired move, so, with that in mind, the predicate ```cpu_movement(+Level, +Color, +Board, +CurrPos, +PrevPos, +Stack, -NBoard)``` was implemented and according to the computer's level finds the most suitable move for the situation given its strategy. This predicate is in all similar to ```move_stack\6```, which was described before in the player's section. It repeats the process of finding a valid move and moving the stack chosen in the placement phase along the board leaving pieces behind its path until this stack is empty. The major differences between the 2 levels of computers shine in the decision of which move to play, however, before that, both strategies must retrieve a list of all possible moves from the given current position, that's the goal of the predicate ```find_moves(+Board, +CurrPos, +PrevPos, -Moves)```. For this task, the predicate ```findall\3``` was once again used, but this time its goal is the predicate ```valid_movement(+Board, +CX/CY, +PX/PY, +NX/NY)```, where CX/CY represent the current position's coordinates, PX/PY the previous one's and NX/NY the new position's. ```valid_movement\4``` checks if the new position is within the board's limits, there is no backtracking to the previous position and that the new position is orthogonally adjacent to the current one, essentially all of the rules that a movement must obey.

Just like in the placement phase, level 1 chooses a random move from the retrieved list from ```find_moves\4```, the magic happens in the level 2 where some consideration is taken before deciding on which move to make.
A level 2 CPU makes use of the ```value_move(+Board, +Bottom, +Color, +[CurrPos|L], +Best-Value, -Move)``` predicate, which iterates through the list of possible positions/moves and computes the value of each. The perceived value of a given position is calculated by summing the values of that position's column and row, which is the amount of ```Color``` colored pieces in each, ```Color``` being the computer's piece color, in whatever position of the stack the piece may be, whether it is the top one or not. 
In addition to this computation and in order to make this difficulty level slightly more intelligent, the starting value of ```Value``` is set to 100 when the piece being placed, ```Bottom```, is not the same color as the computer's, and the goal of the evaluation becomes finding the placement which is the least valuable in regards to the computer's pieces. The idea behind this evaluation procedure is that, when placing a piece, it is better to do so somwehere that is close to pieces of the same color as the player's, since, even if these aren't on the top of their stack, they move essentially in the same directions as the current the one being selected and allow for a better strategic placement and advancement of pieces. Similarly, it is good to avoid placing pieces on locations that are controlled by the player and that is also discouraged by the second part of the method. It is also worth pointing out that placing a piece in a place where there already exists one of the same color is reinforced, as these are accounted for twice in the evaluation of a place, and, just like in the placement phase, this gives us further controls of key stacks.
Whilst this "heuristic" is interesting and certainly more thought out than randomly placing pieces, it is still not perfect or no where near it. The method does not account for positions in a diagonal, is very short sighted and localized, as it is an evaluation done move by move and not in a string of movements, without looking further ahead and considering the repercursions of shifting the position to the moved place, however Qawale is a simple yet deep game and its depth comes exactly from the ramifications a move may have in the medium/long game.

At the end of a computer phase a prompt for input is required to the user operating the computer, this gives an opportunity to reviews the moves that the computer made and better understand how a position came to be.

#### End Game

The predicate ```game_over(+Board, +Player)``` checks if the game as ended with a winner by receiving the current board state and a ```Player```. In this predicate all of the rows, columns and both of the diagonals are verified for the desired "4 in a row" of the ```Player```'s color situation that is the target of the game. When a ```Player``` meets this requirement the ```finish\1``` predicate is ran and a flavored message is displayed, taking into account the type of player, human or computer, and their color, congratulating them on their win.

The end of a game is checked at the start of each player's turn, this ensures that a game can not be won by chance in the middle of the movement phase of a player or by just placing a piece on top a stack without moving it, the validity of both of these scenarios are not contemplated in the game's manual and as such are left open to player interpetation, however the group agreed that these winning conditions left a sour taste for the losing player and the winner. If a game has 0 turns left the game ends in a draw and players are shown a message with that information.

### Conclusion

In conclusion, the developed project has met all of the goals set, it is a rule accurate, which terminates in the designed victory/lose/draw states, implementation of Qawale for 2 players written in Prolog that correctly conveys the present board state of the game and allows for multiple user modes between human and computer and 2 computer levels. Whilst we see some clear areas of improvement, mainly in the implementation of a more intelligent level 2 computer, we are happy and proud of the developed work. This project also shined a light on the strengths and use cases of logic programming languages and allowed a peek into their evolving world.
