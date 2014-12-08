import Data.Char
import System.Random
import Text.Read

-- Simple Variante die Duplikate nicht entfernt MIT do-Notation
nRandomR' range amount = do
	if amount == 0 then do
		return []
	else do
		x <- randomRIO range
		tail <- nRandomR' range (amount - 1)
		return (x : tail)
{-
// Aequivalente Formulierung in (Pseudo-) Java, vergleiche mit do-Variante
List<Integer> nRandomR(Integer low, Integer high, Integer length){
	if(length == 0){
		return new List<Integer>();
	} else {
		Integer x = getRandom(low, high)
		List<Integer> tail = nRandomR(low, high, length - 1);
		tail.push(x);
		return tail;
	}
}
-}

nRandomR :: (Int, Int) -> Int -> IO [Int]
nRandomR range n = do
	let
		go :: Int -> [Int] -> IO [Int]
		go 0 xs = do
			return xs
		go n xs = do
			x <- randomRIO range
			if x `elem` xs
				then do
					ys <- go n       xs
					return ys
				else do
					ys <- go (n - 1) (x : xs)
					return ys
	xs <- go n []
	return xs

getLineInt :: IO Int
getLineInt = do
	line <- getLine
	case readMaybe line of
		Just n  -> do
			return n
		Nothing -> do
			putStrLn "That's not a number! Your guess?"
			n <- getLineInt
			return n

guessNum :: IO Int
guessNum = do
	number <- randomRIO (0,100)
	let
		help :: Int -> Int -> IO Int
		help attempts number = do
			guess <- getLineInt
			case guess of
				x | x > number -> do
					putStrLn "That's too high! Your guess?"
					attempts <- help (attempts + 1) number
					return attempts
				x | x < number -> do
					putStrLn "That's too low! Your guess?"
					attempts <- help (attempts + 1) number
					return attempts
				_ -> do
					putStrLn "That's correct!"
					return attempts
	attempts <- help 0 number
	return attempts

main :: IO ()
main = do
	putStrLn "1 <= x <= 100. Your guess?"
	x <- guessNum
	putStrLn ("It took you " ++ (show x) ++ " tries to get the correct number!")
