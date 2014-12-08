module GuessProgram where
import Network
import Text.Read
import System.Random

{- Start of Main Program -}
main = do
		putStrLn "This is a little guessing program where the user is asked to enter a guess between 0 and 100."
		putStrLn "After each guess you get a hint from the program if your guess was smaller or greater than the wanted number"
		putStrLn "The aim of the game is to guess the wanted number in less guesses as possible"
		x <- guessNum
		putStrLn ("It took you " ++ (show x) ++ " guesses to find the correct number")
{- End of Main Program -}

{- G1 -}
-- generates a list of n unique random numbers within the passed range
-- if n exceeds the number of available numbers within the range the program won't terminate
nRandomR :: (Int, Int) -> Int -> IO [Int]
nRandomR range n = do 
	ys <- (go n []) 
	return ys where
	go :: Int -> [Int] -> IO [Int]
	go 0 xs = do 
			return xs
	go n xs = do 
			x <- randomRIO range 
			if (elem x xs) then do
				ys <- go n xs
				return ys
			else do
				ys <- go (n-1) (x:xs)
				return ys

{- G2 -}
-- asks the user to enter a valid number
-- in the case the user enteres a invalid number the program prints an error message
-- on the screen and asks the user again for a number
getLineInt :: IO Int
getLineInt = do
			putStrLn "Please enter your guess"
			xs <- getLine
			case readMaybe xs of
				Just x -> do
					return x
				Nothing -> do
					putStrLn "Invalid number entered"
					x <- getLineInt
					return x

-- chooses a randomly chosen number from 0 to 100 and asks the user to enter its guess
-- as long as the user has correctly guessed the wanted number
-- after each guess the program gives a hint if the entered guess was smaller or greater than the wanted number
guessNum :: IO Int
guessNum = do
		number <- randomRIO (0,100)
		numberOfGuesses <- go 1 number
		return numberOfGuesses
		where
			go guesses number = do
					guess <- getLineInt
					if guess < number then do
						putStrLn "I'm sorry but your number is too small"
						x <- go (guesses+1) number
						return x
					else if guess > number then do
						putStrLn "I'm sorry but your number is too big"
						x <- go (guesses+1) number
						return x
					else do
						putStrLn "Congratulation you've correctly guessed the number"
						return guesses