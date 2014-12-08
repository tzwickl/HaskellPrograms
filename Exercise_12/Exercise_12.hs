module Exercise_12 where
import qualified Data.ByteString as BS
import qualified Data.Char
import Data.Bits
import Data.Binary
import Data.List.Split
import Data.List
import qualified Huffman as H
import qualified Data.Binary as BI
import System.IO
import System.FilePath
import System.Environment (getArgs)

{- Library -- nicht veraendern -}

data Html =
  Text String |
  Block String [Html]
  deriving Show

html_ex1 = Text "Every string should learn to swim"
html_ex2 = Block "head" []
html_ex3 = Block "body" [Block "p" [Text "My cat"], Text "is not a float"]
html_ex4 = Text "Sei Epsilon < 0"
html_ex5 = Text "Üblicherweise ist 𝜀 > 0"

data Expr = Input Integer |
            Add Expr Expr |
            Sub Expr Expr |
            Mul Expr Expr deriving (Show, Eq)

foldExpr :: (Integer -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
foldExpr f _ _ _ (Input n) = f n
foldExpr f g h i (Add e1 e2) = g (foldExpr f g h i e1) (foldExpr f g h i e2)
foldExpr f g h i (Sub e1 e2) = h (foldExpr f g h i e1) (foldExpr f g h i e2)
foldExpr f g h i (Mul e1 e2) = i (foldExpr f g h i e1) (foldExpr f g h i e2)

{- G12.1 -}

{- Folgende Funktionen können Sie zur Lösung verwenden (andere Funktionen sind natürlich auch gestattet):

    -- definiert in Data.ByteString
    -- entpackt den ByteString in eine Liste von Byte-Werten (Word8 = Byte)
    unpack :: ByteString -> [Word8]

    -- definiert in Data.ByteString
    -- packt eine Liste von Byte-Werten in einen ByteString
    pack :: [Word8] -> ByteString

    -- definiert in Data.Bits
    -- Links-Shift um n Stellen
    shiftL :: Word8 -> Int -> Word8

    -- definiert in Data.ByteString
    -- schreibt einen ByteString in eine Datei
    writeFile :: FilePath -> ByteString -> IO ()

    -- definiert in Data.ByteString
    -- liest einen ByteString aus einer Datei
    readFile :: FilePath -> IO ByteString

-}

compress :: String -> FilePath -> IO ()
compress = undefined

decompress :: FilePath -> IO ( Maybe String )
decompress = undefined

{- G12.2 -}
-- converts a Html structure to a plain text
plainHtml :: Html -> String
plainHtml (Text str) 		= parse str
plainHtml (Block tag xs) 	= "<" ++ tag ++ ">" ++ concat [plainHtml x| x <- xs] ++ "</" ++ tag ++ ">"

-- converts all special characters to their HTML representation
parse :: String -> String
parse []		= ""
parse (s:ss) 
		| Data.Char.ord s >= 128 	= "&#" ++ show (Data.Char.ord s) ++ ";" ++ parse ss
		| s == '&' 					= "&#amp;" 	++ parse ss
		| s == '<'					= "&lt;" 	++ parse ss
		| s == '>'					= "&gt;" 	++ parse ss
		| s == 'ä'					= "&auml;" 	++ parse ss
		| s == 'Ä'					= "&Auml;" 	++ parse ss
		| s == 'ö'					= "&ouml;"	++ parse ss
		| s == 'Ö'					= "&Ouml;"	++ parse ss
		| s == 'ü'					= "&uuml;"	++ parse ss
		| s == 'Ü'					= "&Uuml;"	++ parse ss
		| s == 'ß'					= "&szlig;" ++ parse ss
		| otherwise					= 		   s : parse ss
		
{- H12.1 -}

{- Folgende Funktionen können Sie zur Lösung verwenden (andere Funktionen sind natürlich auch gestattet):

    -- definiert in Data.ByteString
    -- liest von der Standardeingabe, gibt einen Stream von Bytes
    getContents :: IO ByteString

    -- definiert in Data.ByteString
    -- entpackt den ByteString in eine Liste von Byte-Werten (Word8 = Byte)
    unpack :: ByteString -> [Word8]

    -- definiert in Data.Bits
    -- testet, ob ein bestimmtes Bit in einem Byte gesetzt ist
    testBit :: Word8 -> Int -> Bool

-}
-- lookup table with all base64 characters
base64Table :: [(Int, Char)]
base64Table = [
		 ( 0, 'A'), (17, 'R'), (34, 'i'), (51, 'z'),
         ( 1, 'B'), (18, 'S'), (35, 'j'), (52, '0'),
         ( 2, 'C'), (19, 'T'), (36, 'k'), (53, '1'),
         ( 3, 'D'), (20, 'U'), (37, 'l'), (54, '2'),
         ( 4, 'E'), (21, 'V'), (38, 'm'), (55, '3'),
         ( 5, 'F'), (22, 'W'), (39, 'n'), (56, '4'),
         ( 6, 'G'), (23, 'X'), (40, 'o'), (57, '5'),
         ( 7, 'H'), (24, 'Y'), (41, 'p'), (58, '6'),
         ( 8, 'I'), (25, 'Z'), (42, 'q'), (59, '7'),
         ( 9, 'J'), (26, 'a'), (43, 'r'), (60, '8'),
         (10, 'K'), (27, 'b'), (44, 's'), (61, '9'),
         (11, 'L'), (28, 'c'), (45, 't'), (62, '+'),
         (12, 'M'), (29, 'd'), (46, 'u'), (63, '/'),
         (13, 'N'), (30, 'e'), (47, 'v'),
         (14, 'O'), (31, 'f'), (48, 'w'),
         (15, 'P'), (32, 'g'), (49, 'x'),
         (16, 'Q'), (33, 'h'), (50, 'y')]

-- converts a passed ByteString into a base64 string
base64 :: BS.ByteString -> [Char]
base64 bs = if null bytes then "" else convertToBase64 bytes where
	bytes = BS.unpack bs

-- converts all passed Bytes into a base64 string
convertToBase64 :: [Word8] -> [Char]
convertToBase64 [] 			= []
convertToBase64 (x:[])		= getBase64Char (chunksOf 6 (convertToBool x ++ replicate 4 False)) 
								++ "=" ++ "="
convertToBase64 (x:y:[])	= getBase64Char (chunksOf 6 (convertToBool x ++ convertToBool y ++ replicate 2 False)) 
								++ "="
convertToBase64 (x:y:z:xs)  = getBase64Char (chunksOf 6 (convertToBool x ++ convertToBool y ++ convertToBool z)) 
								++ convertToBase64 xs
		
-- converts a bool sequence into a base64 string
-- the bool sequence represents the bits of a string
getBase64Char :: [[Bool]] -> [Char]
getBase64Char []		= ""
getBase64Char (b:bs) 	= ascii : getBase64Char bs where
		(Just ascii) = lookup (evaluateBool b) base64Table

-- converts one Byte into a bool sequence of 8 bools
-- each bool represents one bit of the passed word8
convertToBool :: Word8 -> [Bool]
convertToBool w = map (\x -> testBit w x) [7,6..0]

-- converts a sequence of 6 bools to an integer (ascii representation of a character in the
-- base64 lookup table)
evaluateBool :: [Bool] -> Int
evaluateBool bs = multiMap bs [32,16,8,4,2,1] where
		multiMap :: [Bool] -> [Int] -> Int
		multiMap []		[]		= 0
		multiMap (b:bs) (a:as) 
			| b 		= a + multiMap bs as
			| otherwise =     multiMap bs as

-- reads in the entire input from the user, encodes the user's input with the Base64 encoding scheme 
-- and prints the result to the standard output
main :: IO ()
main = do
	content <- BS.getContents
	let base64String = base64 content
	putStrLn ""
	putStrLn base64String

{- H12.2 -}
-- checks if it is possible to find an expression (add, mul, sub) which computes to the
-- target number
checkExpr :: [Integer] -> Integer -> Maybe Expr
checkExpr inputs target = checkExpressions expr (permutations inputs) target where
		len  = length inputs
		expr = [[Input 0]] ++ (reverse $ generateExpr (len-1))
		
-- takes a list of possible Expressions and a list of possible permutations of the input
-- numbers and checks all possible Expression with all possible combinations of input
-- to calculate the target number
-- if a solution is found the expression is returned else Nothing is returned
checkExpressions :: [[Expr]] -> [[Integer]] -> Integer -> Maybe Expr
checkExpressions []		 _      _    	= Nothing
checkExpressions (e:exp) input  target  = 
	case (go e input) of
		(Just x) -> Just x
		Nothing  -> checkExpressions exp input target
	where
		go _ []			= Nothing
		go e (i:input)
			| result == target 	= Just ie
			| otherwise			= go e input where
				result = foldExpr (\x -> x) (+) (-) (*) ie
				ie     = initializeExpr e i

-- returns the expression which is closest to the target number
approxExpr :: [Integer] -> Integer -> Expr
approxExpr inputs target = 
	checkAppExpressions expr (permutations inputs) target (abs (maximum inputs) + (abs target) + 1, Input 0) where
		len  = length inputs
		expr = [[Input 0]] ++ (reverse $ generateExpr (len-1))

-- takes a list of possible Expressions and a list of possible permutations of the input
-- numbers and checks all possible Expression with all possible combinations of input
-- to calculate the target number
-- returns always a result independent if the target number could be calculated or not
-- if the target number couldn't be calculated the expression with the closest result is returned
checkAppExpressions :: [[Expr]] -> [[Integer]] -> Integer -> (Integer, Expr) -> Expr
checkAppExpressions []		 _     _    	 (_,res)			= res
checkAppExpressions (e:exp) input  target    (d@(diff, ex)) 
	| diff' < diff = checkAppExpressions exp input target (diff',res)
	| otherwise    = checkAppExpressions exp input target d where
		(diff', res) = go e input d where
			go _ []	       diff				= diff
			go e (i:input) (d@(diff, ex))
				| diff' < diff 	= go e input (diff', ie)
				| otherwise		= go e input d  where
					diff'  = abs (target - result)
					result = foldExpr (\x -> x) (+) (-) (*) ie
					ie     = initializeExpr e i

-- initialise the passed list of expression with the input numbers and returns the
-- expression with the numbers
initializeExpr :: [Expr] -> [Integer] -> Expr
initializeExpr []			   	(i:[])		 	= Input i
initializeExpr (Input x  :[])   (i:input)		= Input i
initializeExpr ((Add x y):[])	(i1:i2:input)	= Add (Input i1) (Input i2)
initializeExpr ((Sub x y):[])	(i1:i2:input)	= Sub (Input i1) (Input i2)
initializeExpr ((Mul x y):[])	(i1:i2:input)	= Mul (Input i1) (Input i2)
initializeExpr ((Add x y):exp) 	(i:input) 		= Add (Input i) (initializeExpr exp input)
initializeExpr ((Sub x y):exp) 	(i:input) 		= Sub (Input i) (initializeExpr exp input)
initializeExpr ((Mul x y):exp) 	(i:input) 		= Mul (Input i) (initializeExpr exp input)

-- generates all possible combination of Expressions with the passed length
generateExpr :: Int -> [[Expr]]
generateExpr 0		= []
generateExpr number = permOfLength [Add (Input 0) (Input 0), Sub (Input 0) (Input 0), 
									Mul (Input 0) (Input 0)] number ++ generateExpr (number - 1)
									
-- generates all possible permutations of a list with the passed length
permOfLength :: [a] -> Int -> [[a]]
permOfLength alphabet n = wol alphabet n where
    wol as 0 = [[]]
    wol as n = [[a] ++ s | a <- as, s <- wol as (n - 1)]