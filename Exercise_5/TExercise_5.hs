module TExercise_5 where

import Data.List
import Test.QuickCheck

{----------------------------------------------------------------------------------------}
{- G1 -}
{-
head (tail (map gg (zip [xx, xx] [yy, yy])))
= head (tail (map gg ([(xx, yy), (xx, yy)])))
= head (tail ([gg(xx, yy), gg(xx, yy)]))
= head ([gg(xx, yy)])
= gg[xx, yy]
-}
{----------------------------------------------------------------------------------------}
{- G2 -}
{- Teilaufgabe 1 -}

-- implements a function which calculates f^n(x)
iter :: Int -> (a -> a) -> a -> a
iter 0 _ x = x
iter n f x
	| n < 0 = x
	| otherwise = f (iter (n-1) f x)
	
{- Teilaufgabe 1 -}
-- a) implements a function which calculates the nth power of a number x with using
-- the function iter
pow :: Int -> Int -> Int
pow x n = iter n (x *) 1
--				 ^^^^^
-- is called the section of an infix operator (is just an abbreviation for the Lambda
-- function (\x -> x * x))
-- could also be written as: 
-- iter n f 1 where
-- f n = x * n
	
pow' :: Int -> Int -> Int
pow' x 0 = 1
pow' x n = x * pow' x (n-1)
	
-- test property which checks if the function pow works correctly
pow_prop :: Int -> Int -> Property
pow_prop x n = x > 0 && n > 0 ==> pow x n == pow' x n

-- b) implements a function which removes the nth first elements from a list
drop' :: Int -> [a] -> [a]
drop' 0 as = as
drop' n as = iter n f as where
	f [] = []
	f (a:as) = as
	
-- test property which checks the functionality of drop'
drop_prop :: Int -> [Char] -> Property
drop_prop n as = n < length as ==> drop' n as == drop n as

-- c) implements a function which replicates an element a nth times
-- replicate' n x => [x1, x2, xn]
replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' n a = iter n f [] where
	f as = (a:as)
	
-- test property which checks the functionality of replicate'
replicate_prop :: Int -> [Char] -> Property
replicate_prop n as = n > 0 ==> replicate' n as == replicate n as

{----------------------------------------------------------------------------------------}
{- G3 -}
-- implements a function which partitions a list according to a function f
-- all values by which the function f supplies true are going in the first list of the 
-- tupel, the remaining values are going in the second list of the tupel
{- Teilaufgabe 1 -}
-- first version is implemented with recursion
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' f [] = ([], [])
partition' f (a:as) 
	| f a = (a : (fst (partition' f as)), snd (partition' f as))
	| otherwise = (fst (partition' f as), a : (snd (partition' f as)))
	
{-
another way and also faster way to implement partition' is:
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' f [] = ([], [])
partition' f (a:as) = if f a then (a : ts, fs) else (ts, a : fs) where
	(ts, fs) = partition_rec f as
-}

{- Teilaufgabe 2 -}
-- second version is implemented with the help of filters
partition'' :: (a -> Bool) -> [a] -> ([a], [a])
partition'' f as = (filter f as, filter (not . f) as)

{- Teilaufgabe 3 -}
-- test properties which check the functionality of partition' and partition''
partition_prop :: [Int] -> Property
partition_prop xs = length xs > 0 ==> partition' even xs == partition even xs

partition_prop' :: [Int] -> Property
partition_prop' xs = length xs > 0 ==> partition'' even xs == partition even xs

{- Teilaufgabe 4 -}
prop_partition_distrib :: [Int] -> [Int] -> Bool
prop_partition_distrib xs ys = (ts ++ ts', fs ++ fs') == partition' even (xs ++ ys) 
	where
	(ts,  fs)  = partition' even xs
	(ts', fs') = partition' even ys

{- Teilaufgabe 5 -}
{-
The disadvantage of the second implementation with the filter is that you have to 
go through the entire list twice. The first time with the f and the second time with
not f. That means you have a runtime behaviour of O(n^2) while with the first 
implementation with the recursive approach you have only O(n).
At button line the partition' is faster, but partition'' is nicer
-}

{----------------------------------------------------------------------------------------}
{- G4 -}
{-
 - "zeros" ist eine unendliche Liste (Ja, das geht in Haskell!).
 - "length" terminiert fuer unendliche lange Listen nicht.
 - Damit ist "length zeros" undefiniert; mathematisch gesehen koennte man sagen,
   dass die Laenge gegen unendlich geht.
 - Die Subtraktion von "x" auf beiden Seiten von "x + y= x + z" ist nur erlaubt,
   wenn "x" endlich ist. Andernfalls stoesst man auf dieselben Probleme wie beim
   Dividieren durch 0.
 - Damit ist der letzte Schritt (das Subtrahieren von "length zeros" auf beiden
   Seiten) nicht erlaubt.
 - Aus einer falschen Behauptung kann man bekanntlich (mit etwas Argumentations-
   kunst) alles folgern, z.B. auch "0 = 1".
-}

-- function which has the functionality to separate a string according to a
-- delimiter -> sep ',' "Thomas,Daniel,Fabian" => ["Thomas","Daniel","Fabian"]
sep :: Char -> [Char] -> [[Char]]
sep _ [] = []
sep a as = [xs] ++ (sep a (delete ' ' (delete a (delete' xs as)))) where
	xs = f a as
	delete' [] xs = xs
	delete' (a:as) (x:xs) = if a == x then delete' as xs else xs
	f _ [] = []
	f a (x:xs) = if x /= a then x : f a xs else []