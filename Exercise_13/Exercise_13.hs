module Exercise_13 where
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Binary as DB
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.List
import Control.Monad
import Control.Arrow
import Data.Maybe
import Test.QuickCheck

{- G13.2 -}
{-
inf :: a -> [ a ]
inf x = x : inf x

f :: [ Int ] -> [ Int ] -> [ Int ] -> Int
f ( x : xs ) ( y : ys ) zs | x > y = y
f ( x1 : x2 : xs ) ys ( z : zs ) = x1

Betrachten Sie die Aufrufe:

f (         inf (1+0)) (         inf (1+1)) (         inf (1+2))
f ( (1+0) : inf (1+0)) ( (1+1) : inf (1+1)) (         inf (1+2))	==> evaluate >
f ( 1     : inf 1    ) ( 2     : inf 2    ) (         inf (1+2)) 	==> 1 > 2 = false
f ( 1 : 1 : inf 1    ) ( 2     : inf 2    ) ( (1+2) : inf (1+2))    ==> 	  = 1


f (         inf (1+2)) (         inf (1+1)) ( inf (1+0))
f ( (1+2) : inf (1+2)) ( (1+1) : inf (1+1)) ( inf (1+0))			==> evaluate >
f ( 3     : inf 3    ) ( 2     : inf 2    ) ( inf (1+0))			==> 3 > 2 = true => = 2

f (                 inf (1+0)) [] (         inf (1+1))
f ( (1+0) :         inf (1+0)) [] (         inf (1+1))				==> second list is empty
f ( (1+0) : (1+0) : inf (1+0)) [] ( (1+1) : inf (1+1))				==> = (1+0)

-}

{- G13.3 -}

{- Teilaufgabe 1 -}
-- generates an infinity list of all fibonacci numbers
fib1 :: [Integer]
fib1 = 0 : 1 : zipWith (+) fib1 (tail fib1)

{- Teilaufgabe 2 -}
{-
fib1 = 0 : 1 : zipWith (+) (0 : 1 : zipWith ...) (1 : zipWith ...)
fib1 = 0 : 1 : (0+1) : zipWith (+) (1 : (0+1) : zipWith ...) ((0+1) : zipWith ...)
fib1 = 0 : 1 : 1     : zipWith (+) (1 : 1 : zipWith ...) (1 : zipWith ...)
fib1 = 0 : 1 : 1 : (1+1) : zipWith (+) (1 : (1+1) : zipWith ...) ((1+1) : zipWith ...)
fib1 = 0 : 1 : 1 : 2     : zipWith (+) (1 : 2 : zipWith ...) (2 : zipWith ...)
fib1 = 0 : 1 : 1 : 2 : (1+2) : zipWith (+) (2 : (1+2) : zipWith ...) ((1+2) : zipWith ...)
fib1 = 0 : 1 : 1 : 3         : zipWith (+) (2 : 3 : zipWith ...) (3 : zipWith ...)
-}

{- Teilaufgabe 3 -}
{-
fib2 :: Integer -> Integer -> [ Integer ]
fib2 m n = m : fib2 n ( m + n )

fib2 0 1
fib2 0 : fib2 1 (0+1)
fib2 0 : 1 : fib2 (0+1) (1 + (0+1))
fib2 0 : 1 : 1 : fib2 1 (1+1)
fib2 0 : 1 : 1 : fib2 (1+1) (1 + (1+1))
fib2 0 : 1 : 1 : 2 : fib2 (2+1) ((2+1) + 2)
-}


{- Teilaufgabe 4 -}
{-
fib 0 = 0
fib 1 = 1
fib n = fib ( n - 1) + fib ( n - 2)

fib 4
fib (fib 3) + (fib 2)
fib (fib 2 + fib 1) + (fib 1 + fib 0)
fib ((fib 1 + fib 0) + 1) + (1 + 0)
fib ((1 + 0) + 1) + (1 + 0)
= 3
-}

{- G13.4 -}

mix :: [[a]] -> [a]
mix = undefined


{- H13.1 -}
-- generates an infinite list with all combinations of the input
wordsOf :: [a] -> [[a]]
wordsOf alphabet = go 0 where
	go n = permOfLength alphabet n ++ go (n+1)

-- generates all possible permutations of a list with the passed length
permOfLength :: [a] -> Integer -> [[a]]
permOfLength alphabet n = wol alphabet n where
    wol as 0 = [[]]
    wol as n = [[a] ++ s | a <- as, s <- wol as (n - 1)]

{- H13.2 -}
-- returns length of the passed list lists where the first list contains each first element
-- the second list contains each second element, the third list contains each third element
-- from the list and so forth
skipper :: [a] -> [[a]]
skipper list = go 1 where
	go n | not $ null nElem = [nElem] ++ go (n+1)
		 | otherwise  = [] where
		nElem = takeNthElement n list
	
-- returns each nth element in the passed list	
takeNthElement :: Integer -> [a] -> [a]
takeNthElement _ [] = []
takeNthElement n ss = go n ss where
		go 1 (c:cs) = c : go n cs
		go n []		= []
		go n (c:cs) = go (n-1) cs

{- H13.3 -}
{- Teilaufgabe 1 -}
-- same functionality as the lookup function but implemented with a filter
lookupByFilter :: Eq a => a -> [(a, b)] -> Maybe b
lookupByFilter k xs = evaluate maybeList where
	maybeList 				= listToMaybe $ filter (\(x,y) -> x == k) xs
	evaluate (Just (x,y)) 	= Just y
	evaluate Nothing	  	= Nothing
	
{- Teilaufgabe 2 -}
{-
After testing both functions with different examples I came to the conclusion that the 
lookup function defined by Data.List is slightly more efficient especially in dealing
with long lists where the desired element is situated at the end of the list. The problem
I guess is that the filter function does take a boolean function as a first argument 
which has to be invoked to each element which does take slightly longer
(because a function needs to be invoked which in turn needs extra memory on the stack)
than the usual equal (==) operator that is used in the lookup function defined by Data.List.
For the rest they are equally because of the laziness of Haskell both functions terminate
as soon as they've found the first matching element and return them. So both function
can also deal with infinite lists and will always terminate as long as the list does 
consist the desired element
-}

{- Teilaufgabe 3 -}
-- tests if the lookupByFilter function results in the same result as the lookup function
-- defined in Data.List
propLookup :: [(Int, String)] -> Int -> Bool
propLookup xs x = lookupByFilter x xs == lookup x xs
	
{- H13.4 -}
compress :: BS.ByteString -> BS.ByteString
compress byteStr = byteStr

decompress :: BS.ByteString -> BS.ByteString
decompress byteStr = byteStr