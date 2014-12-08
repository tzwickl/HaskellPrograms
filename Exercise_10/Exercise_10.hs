module Exercise_10 where
import Network
import Text.Read (readMaybe)
import System.Random
import System.IO
import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (intercalate)

{- Library - nicht veraendern -}

type Point = (Rational, Rational)
type Polygon = [Point]
data City = City String Point Integer deriving Show
data District = District Polygon [City] deriving Show

{- Ende Library -}

{- G1 -}
-- generates a list of n unique random numbers within the passed range
-- if n exceeds the number of available numbers within the range the program won't terminate
nRandomR :: (Int, Int) -> Int -> IO [Int]
nRandomR range n = do 
	go n [] where
	go :: Int -> [Int] -> IO [Int]
	go 0 xs = do 
			return xs
	go n xs = do 
			x <- randomRIO range 
			if (elem x xs) then do
				go n xs
			else do
				go (n-1) (x:xs)

{- G2 -}
-- asks the user to enter a valid number
-- in the case the user enteres a invalid number the program prints an error message
-- on the screen and asks the user again for a number
getLineInt :: IO Int
getLineInt = do
			putStrLn "Please enter a number"
			line <- getLine
			case readMaybe line of
				Just x -> do
					return x
				Nothing -> do
					putStrLn "Invalid number entered"
					getLineInt

-- chooses a randomly chosen number from 0 to 100 and asks the user to enter its guess
-- as long as the user has correctly guessed the wanted number
-- after each guess the program gives a hint if the entered guess was smaller or greater than the wanted number
guessNum :: IO Int
guessNum = do
		number <- randomRIO (0,100)
		go 1 number
		where
			go guesses number = do
					guess <- getLineInt
					if guess < number then do
						putStrLn "I'm sorry but your number is too small"
						go (guesses+1) number
					else if guess > number then do
						putStrLn "I'm sorry but your number is too big"
						go (guesses+1) number
					else do
						putStrLn "Congratulation you've correctly guessed the number"
						return guesses

{- H1 -}
-- counts the lines which does contain valid numbers and terminates when the user enters an empty line
countNumbers :: IO Int
countNumbers = do
				count 0 where
				count :: Int -> IO Int
				count n = do
						line <- getLine
						case line of
							"" -> do
								return n
							_  -> case readMaybe line :: Maybe Int of
										Just _ -> do
											count (n+1)
										Nothing -> do
											count n
	
{- H2 -}
-- joins two files and returns a list of lines which represents the content of a file
-- the first FilePath must be a file with e-mail addresses and names
-- the second FilePath must be a file with e-mail addresses and a list of all points the respective student has gained
joinFiles :: FilePath -> FilePath -> IO [String]
joinFiles file1 file2 = do
					f1 <- openFile file1 ReadMode
					f2 <- openFile file2 ReadMode
					content1 <- hGetContents f1
					content2 <- hGetContents f2
					let result = processString (lines content1) (lines content2)
					return result

-- joins all lines of a file where the e-mail address does comply	
processString :: [String] -> [String] -> [String]
processString [] _ = []
processString _ [] = []
processString (l1:ls1) (l2:ls2) 
	| head lineFile1 == head lineFile2 = [intercalate "," (lineFile1 ++ (tail lineFile2))] ++ processString ls1 ls2 
	| head lineFile1 < head lineFile2 = processString ls1 (l2:ls2)
	| otherwise = processString (l1:ls1) ls2 where
		lineFile1 = splitOn "," l1
		lineFile2 = splitOn "," l2
					
-- main program which takes the path of the two files and prints the joined file on the console
main :: IO ()
main = do
	arguments <- getArgs
	joinedFile <- joinFiles (arguments !! 0) (arguments !! 1)
	putStrLn (show joinedFile)
	
{- H3 -}
-- establishes a connection to the server fp.in.tum.de on Port 8080 and transmits my
-- e-mail address followed with a random chosen Integer
-- the server responses with two numbers which are processed by the function process
-- the result is returned to the server to validate it
-- the server answers either with OK if the result was valid or otherwise with NOT OK
client :: IO ()
client = withSocketsDo $ do
		n <- getLineInt
		h <- connectTo "fp.in.tum.de" (PortNumber 8080)
		hSetBuffering h LineBuffering
		hPutStrLn h ("thomas.zwickl@tum.de " ++ (show n))
		message <- hGetLine h
		putStrLn ("Message from Server: " ++ message)
		values <- hGetLine h
		putStrLn ("Values from Server: " ++ values)
		let res = process (splitOn " " values) n
		putStrLn ("Send Result to Server (n^a - b): " ++ (show res))
 		hPutStrLn h ("The Result is: " ++ (show res))
		respond <- hGetLine h
		hClose h
		putStrLn ("Reply from Server: " ++ respond)
		
-- process the reply from the Server and returns the result of n^a - b
process :: [String] -> Int -> Int
process args n = n ^ a - b where
	a = eval (readMaybe (args !! 0))
	b = eval (readMaybe (args !! 1))
	eval (Just x) = x
	eval Nothing = 0

{- H4 -}

{-WETT-}
-- splits a district into n triangles
split :: District -> Integer -> [Polygon]
split (District p cs) n = triangulate p n

-- splits a polygon into n triangles
triangulate :: Polygon -> Integer -> [Polygon]
triangulate _ 0 = []
triangulate r 1 = [r]
triangulate (p1:p2:p3:r) n = [[p1,p2,p3]] ++ triangulate ([p1]++[p3]++r) (n-1)
{-TTEW-}
