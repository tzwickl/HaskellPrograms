module Exercise_5 where

import Data.List
import Test.QuickCheck

{- H1 -}
{- Wettbewerbsaufgabe von Blatt 4 -}

type DistanceTable = [(String, Integer, String)]

table :: DistanceTable
table =
    [	("Cape Town", 1660, "Durban"),
		("Cape Town", 1042, "East London"),
		("Cape Town", -1, "Johannesburg"),
		("Durban", 667, "East London"),
		("Durban", 598, "Johannesburg"),
		("East London", 980, "Johannesburg")]


{-WETT-}
isDistanceTableSane :: DistanceTable -> Bool
isDistanceTableSane tab = and [
	(dis2 > 0) &&											-- Nichtnegativität
	(c1 /= c2) &&											-- Nichtreflexivität
	((c1 /= c3) || (c2 /= c4)) && 							-- Konsistenz
	((c1 /= c4) || (c2 /= c3)) &&							-- Konsistenz (Symmetrie)
	if (c2 == c3) then (checkComp tab c1 c4 (dis1+dis2)) 
	else if (c1 == c4) then checkComp tab c2 c3 (dis1+dis2) 
	else if (c2 == c4) then checkComp tab c1 c3 (dis1+dis2)
	else if (c1 /= c3) then checkComp tab c1 c3 (-1)
	else if (c2 /= c4) then checkComp tab c2 c4 (-1) 
	else True												-- Dreiecksregel/Vollständigkeit
	|(c1, dis1, c2) <- tab, (c3, dis2, c4) <- tab, 
	c1 /= c4 || c2 /= c3 || dis1 /= dis2 || c1 == c2,
	c1 /= c3 || c2 /= c4 || dis1 /= dis2 || c1 == c2] where 
	checkComp [] _ _ _ = False
	checkComp (t:ts) c1 c2 (-1) = if ((c3 == c1) && (c4 == c2)) || ((c3 == c2) && (c4 == c1))
		then True else checkComp ts c1 c2 (-1) where (c3,dis2,c4) = t
	checkComp (t:ts) c1 c2 dis1 = if ((c3 == c1) && (c4 == c2)) || ((c3 == c2) && (c4 == c1))
		then dis1 > dis2 else checkComp ts c1 c2 dis1 where (c3,dis2,c4) = t
{-TTEW-}

{- H2 -}

fixpoint :: (a -> a -> Bool) -> (a -> a) -> a -> a
fixpoint eq f x = if (eq) x (f x) then x else fixpoint eq f (f x)

{- H3 -}
{- Teilaufgabe 1 -}
-- comp computes all possible compositions of two relations and don't remove any
-- duplicates
comp :: Eq b => [(a, b)] -> [(b, c)] -> [(a, c)]
comp r1 r2 = [(a, c)| (a, b1) <- r1, (b2, c) <- r2, b1 == b2]

{- Teilaufgabe 2 -}
-- trancl computes the transitive closure of a relation
-- dupliFree removes all duplicated tuples in the input relation
-- union unifies two relations and removes all duplicated tuples in the second
-- diff removes all elements from r1 that are also in r2
-- relation
trancl :: Ord a => [(a, a)] -> [(a, a)]
trancl r = if null (diff (comp r' r') r') then sort $ union r' (comp r' r')
	else trancl (union r' (comp r' r')) where
	union r1 r2 = [x| x <- r1] ++ [y| y <- r2, not(elem y r1)]
	diff r1 r2 = [x| x <- r1, not(elem x r2)]
	r' = dupliFree r
	dupliFree [] = []
	dupliFree (r:rs) = if elem r rs then dupliFree rs else r : dupliFree rs

{- Teilaufgabe 3 -}
-- same functionality as the preceding implementation but this time without recursion
trancl' :: Ord a => [(a, a)] -> [(a, a)]
trancl' r = sort $ nub (fixpoint (==) comp' r) where
	comp' [] = []
	comp' r = union r (comp r r) where
		union r1 r2 = [x| x <- r1] ++ [y| y <- r2, not(elem y r1)]
	
-- quickCheck property to check if the two implementations have the same functionality
trancl_prop :: [(Int, Int)] -> Property
trancl_prop r = length r > 0 ==> trancl r == trancl' r

{- Teilaufgabe 4 -}
-- the first quickCheck property checks if all elements of the input set are still in the
-- output set of the function trancl (input set must be a subset of the output set)
trancl_prop' :: [(Int, Int)] -> Property
trancl_prop' r = length r > 0 ==> subset r (trancl r) where
	subset [] _ = True
	subset (r:r1) r2 = elem r r2 && subset r1 r2
	
-- the second quickCheck property checks if all elements in the output set are really
-- transitive relations
-- A relation R on a set X is transitive if, for all x,y,z in X, whenever x R y and y R z 
-- then x R z
trancl_prop'' :: [(Int, Int)] -> Property
trancl_prop'' r = length r > 0 ==> transitivity (trancl r) where
	-- if element (x,y) exists in r1 and there is also an element (y,z) in r1,
	-- then there must also exist an element (x,z) in the output set of trancl
	transitivity r1 = subset [(x,z)|(x,y1) <- r, (y2,z) <- r1, y1 == y2] r1 where
		subset [] _ = True
		subset (r:r1) r2 = elem r r2 && subset r1 r2
	

{- H4 -}

mapIndex :: (Integer -> a -> b) -> [a] -> [b]
mapIndex f xs = mapIndex' f 0 xs

mapIndex' :: (Integer -> a -> b) -> Integer -> [a] -> [b]
mapIndex' _ _ [] = []
mapIndex' f n (x:xs) = f n x : mapIndex' f (n+1) xs
