import Data.List
import Test.QuickCheck

{------------------------------ Vorbemerkungen --------------------------------}
-- Statt ...
list1 :: [Int]
list1 = map f [1..10] where f x = x * 2

-- geht auch:
list2 :: [Int]
list2 = map (* 2) [1..10]
--          ^^^^^
-- Das ist eine sog. Section, siehe
-- http://www.haskell.org/haskellwiki/Section_of_an_infix_operator

{- Funktionen koennen konkateniert werden -}
addThreeAndMultiplyByFive :: Int -> Int
addThreeAndMultiplyByFive = multiplyByFive . addThree where
	multiplyByFive x = x * 5
	addThree x = x + 3

{------------------------------------  G1 -------------------------------------}

{-
	Schrittweise Auswertung liefert:

	head (tail (map gg (zip [xx , xx] [yy , yy]))) ==
	head (tail (map gg ([(xx, yy), (xx, yy)]))) ==
	head (tail [gg (xx, yy), gg (xx, yy)]) ==
	head [gg (xx, yy)] ==
	gg (xx, yy)
-}

{------------------------------------  G2 -------------------------------------}

{- Idee: Zuerst mal die rekursiven Varianten... -}
pow_rec :: Int -> Int -> Int
pow_rec n 0 = 1
pow_rec n k = n * pow n (k - 1)

drop_rec :: Int -> [a] -> [a]
drop_rec 0 xs = xs
drop_rec n xs = drop_rec (n - 1) (tail xs)

replicate_rec :: Int -> a -> [a]
replicate_rec 0 x = []
replicate_rec n x = x : replicate (n - 1) x

{- ... und dann einige Auswertungsbeispiele

pow_rec 2 3 ==
2 * pow_rec 2 2 ==
2 * 2 * pow_rec 2 1 ==
2 * 2 * 2 * pow_rec 2 0 ==
2 * 2 * 2 * 1 ==
8

drop_rec 2 [1,2,3] ==
drop_rec 1 (tail [1,2,3]) ==
drop_rec 0 (tail (tail [1,2,3]) ==
tail (tail [1,2,3]) ==
tail [2,3] == [3]

replicate_rec 2 7 ==
7 : replicate_rec 1 7 ==
7 : 7 : replicate_rec 0 7 ==
7 : 7 : [] == [7,7]

Coincidence? I think not...
-}

iter :: Int -> (a -> a) -> a -> a
iter n f x
	| n <= 0    = x
	| otherwise = iter (n - 1) f (f x)

{- Mit Sections ... -}
pow :: Int -> Int -> Int
pow n k = iter k (* n) 1

{- ... und ohne -}
pow' :: Int -> Int -> Int
pow' n k = iter k go 1 where
	go x = n * x

{- Usage: quickCheckWith (stdArgs { maxSize = 30 }) prop_pow -}
prop_pow :: Int -> Int -> Property
prop_pow n k = (k >= 0) ==> (pow n k == n ^ k)

drop' :: Int -> [a] -> [a]
drop' n xs = iter n tail xs

prop_drop' :: Eq a => Int -> [a] -> Property
prop_drop' k xs = k <= n ==> drop' k xs == drop k xs where
	n = length xs

{- Mit Sections ... -}
replicate' :: Int -> a -> [a]
replicate' n x = iter n (x :) []

{- ... und ohne -}
replicate'' :: Int -> a -> [a]
replicate'' n x = iter n go [] where
	go xs = x : xs

{- Usage: quickCheckWith (stdArgs { maxSize = 30 }) prop_replicate' -}
prop_replicate' :: Eq a => Int -> a -> Bool
prop_replicate' n x = replicate' n x == replicate n x

{------------------------------------  G3 -------------------------------------}

partition_filter :: (a -> Bool) -> [a] -> ([a], [a])
partition_filter f xs = (filter f xs, filter (not . f) xs)

{- Es gilt: sumAndProduct [1..10] == (55,3628800) -}
sumAndProduct :: [Int] -> (Int, Int)
sumAndProduct [] = (0, 1)
sumAndProduct (x : xs) = (x + s, x * p) where
	(s, p) = sumAndProduct xs

partition_rec :: (a -> Bool) -> [a] -> ([a], [a])
partition_rec f [] = ([], [])
partition_rec f (x : xs) = if f x then (x : ts, fs) else (ts, x : fs) where
	(ts, fs) = partition_rec f xs

prop_partition_rec :: [Int] -> Bool
prop_partition_rec xs = partition_rec even xs == partition even xs

prop_partition_filter :: [Int] -> Bool
prop_partition_filter xs = partition_filter even xs == partition even xs

prop_partition_distrib :: [Int] -> [Int] -> Bool
prop_partition_distrib xs ys = (ts ++ ts', fs ++ fs') == partition_rec even (xs ++ ys) where
	(ts,  fs)  = partition_rec even xs
	(ts', fs') = partition_rec even ys

{- zu zu G5.3.5: partition_rec ist schneller, partition_filter schoener ;-) -}

{------------------------------------  G4 -------------------------------------}

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
