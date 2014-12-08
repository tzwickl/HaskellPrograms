----------------------------------- Module Description -----------------------------------
{-WETT
  The basic idea behind the AI algorithm is that it works with the minimax technique
  The minimax technique relies on the conceptually simple idea of game trees.
  A game tree is an expansion of the total state space of a game. Each node of the
  tree contains the complete description of one state of the game.
  The minMaxTree function traverses recursively n steps down the game tree and simulates all possible
  moves within this path down to the bottom. As more moves are possible as longer 
  takes the program execution, that's why I have restricted the traversing to four steps.
  Suppose I would like to use it to play a game as the first player. Since I am the first player, 
  I'm the maximizing player. The ultimate goal is to find which of the children of the root 
  yields the highest score. To compute this, we consider each of the children individually, 
  and apply the minimax procedure to them one-by-one. However, since we are now on the second 
  level, we must play the part of the minimizing player, whose turn it now is. 
  Therefore, we try to find the node that yields the lowest score. 
  We continue this way down the tree, alternating between maximizing and minimising, 
  until we reach the bottom. The scores are then propagated up through the tree until we 
  get back to the root. Now, the score of each child is known, and we can choose the best move: 
  the greatest if we are the maximizing player, or the least if we are the minimizing. 
  Between each iteration I add the value of the advantage function which calculates
  the mobility of the player and the pice difference.
  Furthermore I add to each value the heuristic weight that has been determined 
  experimentally and base on the advantage you gain when you occupy one of the 
  squares. For example corners are very popular because they're secure and cannot be
  got lost once you've gained them. That's why corners does have a higher weight than 
  all the other squares. Through this concept it is guaranteed that we can make a good
  decision although we don't traverse through the entire games tree to find the 
  globally best solution.
  source: http://www.mkorman.org/othello.pdf
-}

module Othello_Player (State, startState, nextState, main) where
import qualified Data.Map as M
import qualified Data.List as DL
import System.IO
import Data.Char

--------------------------------- Data Type definitions ----------------------------------

-- position on the game board (0,0) is the upper left corner and (7,7) the bottom right corner
type Position = (Int, Int)

-- represents the color of the computer (AI)
type MyColor = Color

-- represents the discs used in this game
-- a square can either be empty or occupied by a disc which can either have the colour
-- Black or White
data Disc = Disc Color | Empty
	deriving Eq
	
-- defines how discs are visualised on the console
instance Show Disc where
	show (Disc Black) = "B"
	show (Disc White) = "W"
	show Empty		  = " "
	
-- defines the two colours of the discs
data Color = Black | White
  deriving (Eq, Show)

-- defines either the user passes or it plays to the given coordinate
data Move = Pass | Play Position
  deriving (Eq, Show)
  
-- defines the game tree
data GameTree = GameTree Position Int [GameTree]
  deriving (Eq, Show)

-- the data type state represents which Position on the game board is occupied by which color
data State = State (M.Map Position Disc) MyColor GameTree

-- defines how the board is visualised on the console
instance Show State where
	show state = boardToAscii state

-- defines when two states are equal
instance Eq State where
	(==) (State map1 myColor1 _) (State map2 myColor2 _) = M.elems map1 == M.elems map2 && myColor1 == myColor2
  
-------------------------------------- User Interface ------------------------------------

startState :: Color -> State
nextState  :: State -> Move -> (Move, State)

-------------------------- Implementation of the Interface -------------------------------

-- initialise the game board and sets the computer's color
startState whoAmI = State map mCol (minMaxTree (Disc Black) (0,0) state) where
	(state@(State map mCol gT)) = setInitialGameBoard whoAmI

-- takes as a first argument the current state and as a second argument the player's move
-- at first it is checked if the player has chosen a valid move
-- if the player's move was valid it is processed and afterwards the computer calculates
-- its best move
-- else the oldState is returned with Pass
-- if the player has passed the computer calculates its move directly
-- after both moves has been processed the new state is returned with the computer's move
-- if the computer cannot find a legal/valid move Pass is returned with the new state
nextState (oldState@(State map oCol gT)) Pass = calculateBestMove (Disc oCol) oldState
nextState (oldState@(State map oCol gT)) move 
				| oldState == newState = (Pass, oldState)
				| otherwise 		   = calculateBestMove (Disc oCol) newState where
						newState = processMove False move mCol oldState
						mCol 	 = getOpponentCol (Disc oCol)

----------- auxiliary functions (not accessible from outside of this module) -------------
  
-- initialise an 8x8 game board with 64 empty squares (game grid) and the computer's color
initializeGameBoard :: MyColor -> GameTree -> State
initializeGameBoard mCol gT = State (ins M.empty [((x,y), Empty)| x <- [0..7], y <- [0..7]]) mCol gT where
		ins map []				= map
		ins map ((pos, col):xs) = ins (M.insert pos col map) xs

-- sets the four initial discs on the grid
setInitialGameBoard :: MyColor -> State
setInitialGameBoard mCol = upd state [((3,3), Disc White), ((4,4), Disc White), 
									  ((3,4), Disc Black), ((4,3), Disc Black)] where
		state		   				= initializeGameBoard mCol (GameTree (0,0) 0 [])
		upd   st [] 				= st
		upd   st ((pos, col):xs) 	= upd (changeColOnPos pos col st) xs
		
-- main function which processes the move
-- if the boolean flag is set to true the game tree is ignored (for initialisation of the game tree)
-- if the boolean flag is set to false the game tree is pruned after each move
-- checks if the passed move is a legal/valid move
-- if the move was legal the position is marked as occupied by your color and all discs 
-- sandwiched by the two discs of the same colour are flipped and the new state is returned
-- else the original state is returned without changes
processMove :: Bool -> Move -> Disc -> State -> State
processMove flag (Play pos) col (st@(State sMap mCol (GameTree _ _ trees))) 
		| flag					 = flipAllDiscs flipDiscs (changeColOnPos pos col st)
		| not $ null $ flipDiscs = flipAllDiscs flipDiscs (changeColOnPos pos col (State sMap mCol newTree))
		| otherwise  			 = st where
			flipDiscs = isPosValid pos col st
			flipAllDiscs []     st	= st
			flipAllDiscs (p:ps) st 	= flipAllDiscs ps (flipDiscOnPos p st)
			newTree					= getTreeWithPos trees where
				getTreeWithPos ((gT@(GameTree p _ _)):ts) 
					| pos == p  = gT
					| otherwise = getTreeWithPos ts
		
-- changes the color on the passed position to the passed color and returns the new state
changeColOnPos :: Position -> Disc -> State -> State
changeColOnPos pos col (State map mCol gT) = State (M.adjust (\x -> col) pos map) mCol gT where
		
-- flips the disc on the passed position
flipDiscOnPos :: Position -> State -> State
flipDiscOnPos pos st = changeColOnPos pos (getOpponentCol $ getColorOnPos pos st) st

-- returns all valid/legal positions that the passed color can make on the current state
-- if no valid/legal positons are available an empty list is returned
getAllLegalPos :: Disc -> State -> [Position]
getAllLegalPos col st = [(x,y)| x <- [0..7], y <- [0..7], not $ null $ isPosValid (x,y) col st]
		
-- checks if a passed position is a valid position
-- at first it is checked if the desired square is unoccupied
-- a position is valid if there is a adjacent disc with the opponent's color and if there
-- is a disc in the same row, column or diagonal with the passed color and these two 
-- discs surround a straight line of contiguous disc with the opponent's color
-- if the position is valid a list of positions is returned which represents the positions
-- of the discs that can be flipped
-- if the position is invalid an empty list is returned
isPosValid :: Position -> Disc -> State -> [Position]
isPosValid pos col state 
		| not (isOccupied pos state) && isAdjacent pos (getOpponentCol col) state = 
				isRowValid pos col state ++ isColumnValid pos col state
				++ isDiagonalValid pos col state
		| otherwise = []

-- checks if the passed position is already occupied
isOccupied :: Position -> State -> Bool
isOccupied pos st = getColorOnPos pos st /= Empty

-- checks if there is an adjacent disc with the passed color around the passed position
isAdjacent :: Position -> Disc -> State -> Bool
isAdjacent (x,y) col st = or $ map (\p -> getColorOnPos p st == col) [(x+x', y+y')| 
									x' <- [-1,0,1], y' <- [-1,0,1], x+x' <= 7 && x+x' >= 0,
									y+y' <= 7 && y+y' >= 0]
									
-- checks if the passed position is a valid position for the player with the passed color
-- a row is valid if there exist at least one straight occupied line between the new 
-- piece and another piece in the same color with a contiguous line of pieces of the 
-- opponent's color between the two pieces
-- all pieces surrounded by these two pieces are going to be flipped
-- this function returns a list of positions of pieces enclosed by the two pieces that
-- can be flipped
-- if the list is empty the position is invalid
isRowValid :: Position -> Disc -> State -> [Position]
isRowValid (x,y) mCol st = goUp (y-1) [] ++ goDown (y+1) [] where
		oCol = getOpponentCol mCol
		goUp (-1) _	 = []
		goUp   y  p 
			| col == oCol = goUp (y-1) ((x,y):p)
			| col == mCol = p
			| otherwise	  = [] where 
				col = getColorOnPos (x,y) st
		goDown 8  _  = []
		goDown y  p 
			| col == oCol = goDown (y+1) ((x,y):p)
			| col == mCol = p
			| otherwise	  = [] where 
				col = getColorOnPos (x,y) st
			
-- checks if the passed position is a valid position for the player with the passed color
-- a column is valid if there exist at least one straight occupied line between the new 
-- piece and another piece in the same color with a contiguous line of pieces of the 
-- opponent's color between the two pieces
-- all pieces surrounded by these two pieces are going to be flipped
-- this function returns a list of positions of pieces enclosed by the two pieces that
-- can be flipped
-- if the list is empty the position is invalid
isColumnValid :: Position -> Disc -> State -> [Position]
isColumnValid (x,y) mCol st = goLeft (x-1) [] ++ goRight (x+1) [] where
		oCol = getOpponentCol mCol
		goLeft (-1) _	  = []
		goLeft  x 	p 
			| col == oCol = goLeft (x-1) ((x,y):p)
			| col == mCol = p
			| otherwise	  = [] where 
				col = getColorOnPos (x,y) st
		goRight 8	_  	  = []
		goRight x 	p 
			| col == oCol = goRight (x+1) ((x,y):p)
			| col == mCol = p
			| otherwise	  = [] where 
				col = getColorOnPos (x,y) st

-- checks if the passed position is a valid position for the player with the passed color
-- a diagonal is valid if there exist at least one straight occupied line between the new 
-- piece and another piece in the same color with a contiguous line of pieces of the 
-- opponent's color between the two pieces
-- all pieces surrounded by these two pieces are going to be flipped
-- this function returns a list of positions of pieces enclosed by the two pieces that
-- can be flipped
-- if the list is empty the position is invalid
isDiagonalValid :: Position -> Disc -> State -> [Position]
isDiagonalValid (x,y) mCol st = goLU (x-1) (y-1) [] ++ goLD (x-1) (y+1) []
								++ goRU (x+1) (y-1) [] ++ goRD (x+1) (y+1) [] where
		oCol = getOpponentCol mCol
		-- go diagonal to the upper left corner
		goLU (-1)  _   _  	= []
		goLU  _	 (-1)  _	= []
		goLU  x    y   p
			| col == oCol = goLU (x-1) (y-1) ((x,y):p)
			| col == mCol = p
			| otherwise	  = [] where
				col = getColorOnPos (x,y) st
		-- go diagonal to the button left corner
		goLD (-1)  _   _  	= []
		goLD  _	   8   _	= []
		goLD  x    y   p
			| col == oCol = goLD (x-1) (y+1) ((x,y):p)
			| col == mCol = p
			| otherwise	  = [] where
				col = getColorOnPos (x,y) st
		-- go diagonal to the upper right corner
		goRU  8    _   _  	= []
		goRU  _	 (-1)  _	= []
		goRU  x    y   p
			| col == oCol = goRU (x+1) (y-1) ((x,y):p)
			| col == mCol = p
			| otherwise	  = [] where
				col = getColorOnPos (x,y) st
		-- go diagonal to the button right corner
		goRD  8    _   _  	= []
		goRD  _	   8   _	= []
		goRD  x    y   p
			| col == oCol = goRD (x+1) (y+1) ((x,y):p)
			| col == mCol = p
			| otherwise	  = [] where
				col = getColorOnPos (x,y) st

-- returns the color on the passed position
getColorOnPos :: Position -> State -> Disc
getColorOnPos pos (State map _ _) = fromJust $ M.lookup pos map where
		fromJust (Just x) = x
		
-- returns the opponent's color
getOpponentCol :: Disc -> Disc
getOpponentCol (Disc Black) = Disc White
getOpponentCol (Disc White) = Disc Black
getOpponentCol _	        = Empty

------------ Functions for calculating the next move of the computer (AI) ----------------

-- represents the heuristic values for each square on the board
-- these values are determined experimentally
-- returns the heuristic value on the passed position
-- this values are used to bias the decision of the computer
getWeight :: Position -> Int
getWeight (x,y) = (hValues ++ reverse hValues) !! (x + (y * 8)) where
	hValues = [20, -3, 11, 8, 8, 11, -3, 20, -3, -7, -4, 1, 1, -4, -7, -3,
			   11, -4, 2, 2, 2, 2, -4, 11, 8, 1, 2, -3, -3, 2, 1, 8]

-- calculates the maximum advantage for each possible move and takes the move with the
-- highest advantage
calculateBestMove :: Disc -> State -> (Move, State)
calculateBestMove col (st@(State sMap mCol (GameTree pos val trees)))
	| null trees	= (Pass, st)
	| otherwise 	= (Play (bestMove), newState) where
		newState				= processMove False (Play bestMove) col st
		(GameTree bestMove _ _)	= trees !! indexOfMax
		(Just indexOfMax)		= DL.elemIndex (maximum advantages) advantages
		advantages				= map (\tree -> minMax 4 col (State sMap mCol tree)) trees

-- calculates what the passed color's advantage against the opponent is, respective
-- of the number of discs with the passed color
numPiecesAdvantage :: Disc -> State -> Int 
numPiecesAdvantage mCol (State map _ _) = count $ M.elems map where
		oCol = getOpponentCol mCol
		count [] = 0
		count (x:xs) 
			| x == mCol = 1  + count xs
			| x == oCol = -1 + count xs
			| otherwise = 0  + count xs

-- calculates what the passed color's advantage against the opponent is, respective
-- of the number of possible movements
numOptionAdvantage :: Disc -> State -> Int
numOptionAdvantage mCol st = length (getAllLegalPos mCol st) - length (getAllLegalPos oCol st) where
		oCol = getOpponentCol mCol

-- calculates the advantage
advantage :: Disc -> State -> Int
advantage mCol st = numPiecesAdvantage mCol st + 2 * numOptionAdvantage mCol st

-- initialise the game tree with all values for each possible state
-- the game tree isn't completely build up in this function because the game tree 
-- of this game would be too large to compute them in a feasible time
-- that's the advantage of lazy evaluation, the tree is only as far evaluated as it is
-- needed and so you can save a lot of precious calculation time
minMaxTree :: Disc -> Position -> State -> GameTree
minMaxTree (dC@(Disc col)) pos (st@(State _ mCol _)) 
		| col == mCol = GameTree pos (advantage dC st + getWeight pos) trees
		| otherwise   = GameTree pos (-(advantage dC st + getWeight pos)) trees where
		oCol 				= getOpponentCol dC
		allPossibleMovesMe  = getAllLegalPos dC st
		trees 				= map (\pos -> minMaxTree oCol pos (processMove True (Play pos) dC st)) allPossibleMovesMe

-- traverses through the tree and finds the min value when its the opponent's turn and the
-- max value if its the computers turn
-- after n steps the minMax search terminates and calculates the advantage for this move
minMax :: Int -> Disc -> State -> Int 
minMax n (dC@(Disc col)) (st@(State sMap mCol (GameTree pos val ts)))
		| null ts || n == 0 = val
		| mCol == col 		= maximum vs
		| otherwise			= minimum vs where
			vs   = map (\tree -> minMax (n-1) oCol (State sMap mCol tree)) ts
			oCol = getOpponentCol dC

-------- Functions for a better visualisation of the game board on the console -----------
							
-- prints the board to the console
boardToAscii :: State -> String
boardToAscii board = "\n  0 1 2 3 4 5 6 7 \n +-+-+-+-+-+-+-+-+\n" ++
        			 (DL.intercalate "\n +-+-+-+-+-+-+-+-+\n" (map (boardToRow board) [0..7])) ++
        			 "\n +-+-+-+-+-+-+-+-+\n  0 1 2 3 4 5 6 7 \n"

-- prints the board row by row to the console
boardToRow :: State -> Int -> String
boardToRow board row = show row ++ "|" ++
        (DL.intercalate "|" (map (\position -> show (getColorOnPos position board)) 
        ([(x, row) | x <- [0..7]]))) ++
        "|" ++ show row
        
------------------ Main function to execute program on a terminal ------------------------

-- clears all character from the screen
clear_screen :: IO()
clear_screen = putStr "\ESC[2J"

-- main program which is the starting point for each executable program
main :: IO ()
main = do
	hPutStrLn 	stdout "Welcome to the game Othello developed by Thomas Zwickl"
	hPutStrLn 	stdout "At first choose your desired color by entering either Black or White"
	hPutStrLn 	stdout "Please consider that the colour Black starts with the first move"
	hPutStrLn 	stdout "Your input:"
	color    <- hGetLine stdin
	clear_screen
	startCoRoutine color
	
-- initialise the color of the player and the computer and executes the first moves
startCoRoutine :: String -> IO ()
startCoRoutine mCol = do
	case mCol of
		"White" -> 	do
						let state = startState Black
				   		let (cMove, nState) = nextState state Pass
				   		loop cMove nState
		"Black" -> 	do
						let state = startState White
						hPutStrLn stdout (show state)
						nextMove <- getPositionInput
						hPutStrLn stdout "Please wait while the computer calculates its next move"
						let (cMove, nState) = nextState state nextMove
						if nState == state then do
				 			hPutStrLn stderr "The entered position isn't a valid position ..."
				 			clear_screen
				 			startCoRoutine mCol
				   		else do
				 			loop cMove nState
		_		-> 	do
						hPutStrLn stderr "The entered color isn't a valid color ..."
				   		hPutStrLn stdout "Please enter a valid color (Black, White)"
				   		col <- hGetLine stdin
				   		clear_screen
				   		startCoRoutine col

-- the main loop which loops as long as the game runs
loop :: Move -> State -> IO ()
loop move oldState = do
			clear_screen
			hPutStrLn stdout (show oldState)
			hPutStrLn stdout ("The computer's move: " ++ (show move))
			userMove <- getPositionInput
			hPutStrLn stdout "Please wait while the computer calculates its next move"
			let (cMove, nState) = nextState oldState userMove
			if nState == oldState then do
					hPutStrLn stderr "The entered position isn't a valid position ..."
					loop move oldState
			else do
					loop cMove nState

-- asks the user for a valid position on the game board and returns the entered move
getPositionInput :: IO Move
getPositionInput = do
	hPutStrLn stdout "Please enter your next move:"
	hPutStrLn stdout "If you want to set a disc on a position x, y enter: Play (x,y)"
	hPutStrLn stdout "If you cannot find a legal move please enter: Pass"
	move <- hGetLine stdin
	case move of
		"Pass" -> 	do
						return Pass
		str	   -> 	do
						let move = parseString str "Play (x,y)" 0 0
						if move == Pass then do
								hPutStrLn stderr "Invalid input ..."
								getPositionInput
						else do
								return move
								
-- parses the user input 
-- if the string complies the pattern Play (x,y) the x and y values are returned
-- else Pass is returned to indicate that the input was invalid		
parseString :: String -> String -> Int -> Int -> Move
parseString []     str    x y 
		| x > 7 || x < 0 = Pass
		| y > 7 || y < 0 = Pass
		| null str       = Play (x,y)
		| otherwise      = Pass
parseString (s:ss) (c:cc) x y 
		| c == s 					= parseString ss cc x y
		| c == 'x' && isNumber s 	= parseString ss cc (read [s]) y
		| c == 'y' && isNumber s 	= parseString ss cc x (read [s])
		| otherwise 				= Pass