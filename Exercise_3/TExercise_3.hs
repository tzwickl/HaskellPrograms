module TExercise_3 where
import Data.List
import Data.Char (isSpace)

{----------------------------------------------------------------------------------------}
{- G3.1 -}

{- Teilaufgabe 1 -}
-- implements a function which takes a list and a single argument and concatenates them
-- without using the (++) function
snoc :: [a] -> a -> [a]
snoc [] y = y : []
snoc (x:xs) y = x : snoc xs y

{- Teilaufgabe 2 -}
-- implements a function which takes a list and a single argument and iterates through 
-- If the list consists the argument, True is returned (elem)
-- Eq a => denotes that the element a must be contained in the Equal class, that means
-- this element must be comparable
member :: Eq a => [a] -> a -> Bool
member [] _ = False
member (x:xs) a = x == a || member xs a
	
{- Teilaufgabe 3 -}
-- implements a function which takes one list and returns the list without the last
-- element (init)
butlast :: [a] -> [a]
butlast [] = []
butlast [_] = []
butlast (x:xs) = x : butlast xs

{- G3.2 -}

{- Teilaufgabe 1 -}
-- implements a function which takes a list and removes all successive repeating elements
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x : unique (uniqueAuxiliary x xs)

uniqueAuxiliary :: Eq a => a -> [a] -> [a]
uniqueAuxiliary _ [] = []
uniqueAuxiliary y (x:xs) 
	| y == x = uniqueAuxiliary x xs
	| otherwise = x : xs
	
{- Teilaufgabe 2 -}
-- implements a function which takes a list and counts how many successive repeating
-- elements this list contains
uniqueCount :: Eq a => [a] -> [(a, Integer)]
uniqueCount [] = []
uniqueCount (x:xs) = [(x, uniqueCountAuxiliary x xs)] ++ uniqueCount (uniqueAuxiliary x xs)

uniqueCountAuxiliary :: Eq a => a -> [a] -> Integer
uniqueCountAuxiliary _ [] = 1
uniqueCountAuxiliary y (x:xs) 
	| y == x = 1 + uniqueCountAuxiliary x xs
	| otherwise = 1
	
{- G3.3 -}

{- Teilaufgabe 1 -}
-- implements a function which takes a list and a separator and adds the separator 
-- between each element ==> that means afterwards there are n elements and n-1 separators
intersep :: a -> [a] -> [a]
intersep _ [] = []
intersep _ [x] = [x]
intersep s (x:xs) = x : s : intersep s xs

{- Teilaufgabe 2 -}
-- implements a function which takes a list of lists and concatenates the lists with 
-- commas and one "and". If the number of elements is smaller than 2 no "and" is added
andList :: [[Char]] -> [Char]
andList [] = []
andList [x] = x
andList [x, y] = x ++ " and " ++ y
andList [x, y, z] = x ++ " , " ++ y ++ ", and " ++ z
andList (x:xs) = x ++ " , " ++ andList xs

{- G3.4 -}
-- implements a function which calculates all n(n - 1) / 2 pairs according the following
-- pattern: [1, 2, ..., n-1, n] ==> [(1, 2), (1, n-1), (1, n), (2, n-1), (2, n), (n-1, n)]
triangle :: [a] -> [(a, a)]
triangle [] = []
triangle (x:xs) = [(x, y) | y <- xs] ++ triangle xs