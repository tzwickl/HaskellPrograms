import Data.List
import Test.QuickCheck

snoc :: [a] -> a -> [a]
snoc []     y = [y]
snoc (x:xs) y = x : snoc xs y

propSnoc :: Eq a => [a] -> a -> Bool
propSnoc xs y = snoc xs y == xs ++ [y]

member :: Eq a => a -> [a] -> Bool
member y []     = False
member y (x:xs) = x == y || member y xs

propMember :: Eq a => a -> [a] -> Bool
propMember x xs = member x xs == elem x xs

butLast :: [a] -> [a]
butLast []     = []
butLast (x:[]) = []
butLast (x:xs) = x : butLast xs

propButLast :: Eq a => [a] -> Bool
propButLast xs
	| null xs   = butLast xs == []
	| otherwise = butLast xs == init xs

uniq :: Eq a => [a] -> [a]
uniq []     = []
uniq (x:xs) = go x xs where
	go x []     = [x]
	go x (y:ys) = if x == y then go x ys else x : go y ys

propUniq :: Eq a => [a] -> Bool
propUniq xs = uniq xs == map head (groupBy (==) xs)

uniqCount :: Eq a => [a] -> [(a, Integer)]
uniqCount []       = []
uniqCount (x : xs) = go (x, 1) xs where
	go (x,n) []         = [(x,n)]
	go (x,n) (y : ys)
		| x == y    = go (x, n + 1) ys
		| otherwise = (x, n) : go (y, 1) ys

propUniqCount :: Eq a => [a] -> Bool
propUniqCount xs = uniqCount xs == map (\xs -> (head xs, genericLength xs)) (groupBy (==) xs)

intersep :: a -> [a] -> [a]
intersep x []     = []
intersep x (y:[]) = [y]
intersep x (y:ys) = y : x : intersep x ys

propIntersep :: Eq a => a -> [a] -> Bool
propIntersep x xs = intersep x xs == intersperse x xs

andList :: [String] -> String
andList []         = ""
andList (x:[])     = x
andList (x:y:[])   = x ++ " and " ++ y
andList (x:y:z:[]) = x ++ ", " ++ y ++ ", and " ++ z
andList (x:xs)     = x ++ ", " ++ andList xs

triangle :: [a] -> [(a, a)]
triangle []     = []
triangle (x:xs) = [ (x, x') | x' <- xs ] ++ triangle xs

{-
 Alternative Loesung welche die Elemente einfach durchnummeriert und dann nur
 diejenigen behaelt, welche die "kleiner" Relation erfuellen.
-}
triangle' :: [a] -> [(a, a)]
triangle' xs = map convertBack (filter smallerCheck [(a, b) | a <- numberedList, b <- numberedList]) where
	numberedList = zip [1..] xs
	smallerCheck ((n, _), (n', _)) = n < n'
	convertBack  ((_, x), (_, x')) = (x, x')

{- Propositions fuer triangle; quickCheck sollte diese alle bestaetigen -}
propTriangleLength :: [a] -> Bool
propTriangleLength xs = length (triangle xs) == n * (n - 1) `div` 2 where n = length xs

propTriangleNumbers :: Integer -> Bool
propTriangleNumbers n = triangle [1..n] == [ (a,b) | a <- [1..n], b <- [1..n], a < b ]

propTriangleAlternative :: Eq a => [a] -> Bool
propTriangleAlternative xs = triangle xs == triangle' xs
