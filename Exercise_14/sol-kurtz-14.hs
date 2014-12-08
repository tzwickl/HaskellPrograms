import Data.List
import Test.QuickCheck

{------------------------------------- G1 -------------------------------------}

{-
1. Ja, ist endrekursiv, das Ergebnis ist entweder eine Konstante oder ein Aufruf von prod.

2. Nein, nicht endrekursiv, das "if" ist zwar kein Problem, die Multiplikation "m * prod ms" schon.

3. Ja, ist endrekursiv, das "if" ist kein Problem, der auesserste Aufruf ist dann "prod".
-}

{------------------------------------- G2 -------------------------------------}

{- Nicht endrekursive Fakultaet -}
fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)

{- Endrekursive Fakultaet -}
fac' :: Integer -> Integer
fac' n = go 1 n where
	go acc 0 = acc
	go acc n = go (acc * n) (n - 1)

{- Schleife, aequivalent zur endrekursiven Variante -}
{-
	while(n != 0){
		acc = acc * n;
		n--;
	}
	return acc;
-}

{- Nicht endrekursive Summe von 1 bis n -}
sum' :: Integer -> Integer
sum' 0 = 0
sum' n = n + sum' (n - 1)

{- Endrekursive Summe von 1 bis n -}
sum'' :: Integer -> Integer
sum'' n = go 0 n where
	go acc 0 = acc
	go acc n = go (acc + n) (n - 1)

{- Nicht endrekursives concat -}
concat' :: [[a]] -> [a]
concat' []         = []
concat' (xs : xss) = xs ++ concat' xss

{- Endrekursives concat -}
concat'' :: [[a]] -> [a]
concat'' xss = go [] xss where
	go acc []         = acc 
	go acc (ys : yss) = go (acc ++ ys) yss

{- Check that all our definitions of concat are equal to the one from the Prelude -}
propConcat :: [[Integer]] -> Bool
propConcat xss = a == b && b == c where
	a = concat xss
	b = concat' xss
	c = concat'' xss

{- Fun fact: Endrekursion hat auch ihre Nachteile -}
infiniteListOfLists = [ [x] | x <- [1..] ]
resultA = take 5 (concat' infiniteListOfLists) -- terminiert
resultB = take 5 (concat'' infiniteListOfLists) -- terminiert nicht

{------------------------------------- G3 -------------------------------------}
{-
map (*2) (1 : threes) !! 1
= ((*2) 1 : map (*2) threes) !! 1
= map (*2) threes !! 0
= map (*2) (3 : threes) !! 0
= ((*2) 3 : map (*2) threes) !! 0
= (*2) 3
= 3*2
= 6

Hinweis:
head []       = error "Prelude.head: empty list"
head (x : xs) = x

(\f -> \x -> x + f 2) (\y -> y * 2) (3 + 1)
= (\x -> x + (\y -> y * 2) 2) (3 + 1)
= (3 + 1) + (\y -> y * 2) 2
= 4 + (\y -> y * 2) 2
= 4 + 2 * 2
= 4 + 4
= 8

head (filter (/=3) threes)
= head (filter (/=3) (3 : threes))
= head (filter (/=3) threes)

(terminiert nicht)
-}
