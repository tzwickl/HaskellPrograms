module Exercise_6 where

import Data.Ratio
import Test.QuickCheck
import Data.List

{- Library -- nicht veraendern -}

type IList = [(Rational, Rational)]

ilist :: Rational -> [Rational] -> IList
ilist n (x:y:z:zs) = (s, s + y) : ilist (s + y + z) zs
    where s = n + x
ilist _ _ = []

rationalGen :: Gen Rational
rationalGen = do
    p <- choose (1,10)
    q <- elements [1,2,3,5,7]
    return $ p % q

ilistGen :: Gen IList
ilistGen = do
    xs <- sized (\size -> resize (3 * size) $ listOf $ rationalGen)
    return $ ilist 0 xs

{- H1 -}
{- Teilaufgabe 1 -}
--checks if an intervall is wellformed according to the following rules:
-- [(x1,y1),(x2,y2)] 
--1. x1 must be less than or equal to y1
--2. y1 must be less than x2
--3. the intervalls must be sorted in ascending order
wellformed :: IList -> Bool
wellformed xs = and ([(y1 < x2)|(x1, y1) <- xs, (x2, y2) <- delete [(x1,y1)] xs] 
	++ [(x <= y)|(x,y) <- xs]) where
	delete [] xs = xs
	delete (y:ys) (x:xs) = if x == y then delete [] xs else delete (y:ys) xs

{- Teilaufgabe 2 -}
--returns an empty list
empty :: IList
empty = []

--checks if a number is in the passed intervall
member :: Rational -> IList -> Bool
member z [] = False
member z (i:ivls) = if (z >= x) && (z <= y) then True else member z ivls where
	(x,y) = i

--inserts a new intervall to an already existing and wellformed intervall list and returns 
--also a wellformed intervall list with the new element if it fits in the list, else the
--original intervall list is returned
insert :: (Rational, Rational) -> IList -> IList
insert y [] = if wellformed [y] then [y] else []
insert y xs = if wellformed (y:xs) then (y:xs) else if wellformed (x') then x' else xs where
	x' = go y xs
	go y [] = []
	go y (x:[]) = if (x1 > y2) then [x] ++ [y] else [x] where
		(x1,y1) = y
		(x2,y2) = x
	go y (x':x'':xs) = if (x1 > y2) && (y1 < x3) then [x'] ++ [y] ++ (x'':xs) 
		else x' : go y (x'':xs) where
		(x1,y1) = y
		(x2,y2) = x'
		(x3,y3) = x''

{-
 - Wenn man QuickCheck ohne weitere Vorbereitungen aufruft, wird es nur in
 - Ausnahmefällen eine wohlgeformte Intervallliste erzeugen. Um dennoch
 - erfolgreich zu testen, haben wir einen sogenannten "Generator" für
 - Intervalllisten geschrieben. Damit wird QuickCheck vorgegeben, wie Werte
 - generiert werden sollen.
 -
 - Sie können die Tests so schreiben wie immer. Wenn Sie die Tests ausprobieren
 - wollen, rufen Sie QuickCheck wie folgt auf:
 -
 - Angenommen, prop ist eine Property, die als erstes Argument eine
 - Intervallliste bekommt. Dann schreiben Sie:
 -
 -      quickCheck (forAll ilistGen prop)
 -}
 
{- Teilaufgabe 3 -}
--checks the correct functionality of the function member by generating a rational 
--number and comparing the result from the member function with them of comparing
--each intervall with the rational number
member_prop :: IList -> Rational -> Property
member_prop ivls r = r >= 0 ==> member r ivls == or [r >= x && r <= y| (x,y) <- ivls]

--checks the correct functionality of the function insert by inserting a random 
--generated element into an already wellformed interval list and checking if it is
--still wellformed after the insertion operation
insert_prop :: IList -> (Rational, Rational) -> Bool
insert_prop ivls r = wellformed (Exercise_6.insert r ivls)

{- H2 -}
--combines the functionality of a map and foldl
foldlMap :: (a -> c -> (b, c)) -> [a] -> c -> ([b], c)
foldlMap _ [] x = ([], x)
foldlMap f (a:as) x = (b:bs, cn) where
	(b,cs) = f a x
	(bs, cn) = foldlMap f as cs

fresh :: String -> [String] -> (String, [String])
fresh s names | elem s names = fresh (s ++ "_") names
fresh s names = (s, s : names)

freshes :: [String] -> [String] -> ([String], [String])
freshes = foldlMap fresh
{- H3 -}

{-WETT-}
count :: a -> Int -> (Int, Int)
count _ j = (j, j+1)

index1 :: [a] -> Int -> ([Int], Int)
index1 as j = foldlMap (count) as j

index2 :: [[a]] -> Int -> ([[Int]], Int)
index2 [] j = ([], j)
index2 (a:as) j = (j':js, jn') where
	(j', jn) = foldlMap (count) a j
	(js, jn') = index2 as jn
{-TTEW-}
