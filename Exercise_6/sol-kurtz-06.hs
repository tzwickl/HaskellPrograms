import Data.List
import Test.QuickCheck

{------------------------------- Vorbemerkungen -------------------------------}

{-

Es gibt den "$" und "." Operator. Ersterer wendet eine Funktion auf einen Wert
an, zweiter verkettet zwei Funktionen. Aus dem GHCi:

*Main> :t ($)
($) :: (a -> b) -> a -> b
*Main> :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c

Es gilt hierbei:

f $ x == f x
(f . g) x == f (g x)

Beispiele:

-}

f x = x + 1

f'  = (+ 1)

g x = x * 2

g'  = (* 2)

h x   = f (g x)

h' x  = (f . g) x

h'' x = f $ g $ x

h'''  = f . g

list  = zipWith ($) [f, f', g, g', h, h', h'', h'''] [42,42..]

list' = map ($ 42) [f, f', g, g', h, h', h'', h''']

{--------------------------------------- G1 -----------------------------------}

{-

1. Links ist die 5 erstes Argument, rechts zweites Argument

	div 5   == \ x -> div 5 x
	`div` 5 == \ x -> div x 5

2. Links ist die 7 zweites Argument, rechts erstes Argument

	+ 7   == \ x -> x + 7
	(+) 7 == \ x -> 7 + x

3. Links wird eine Liste aus Elemente zu einer Liste von Listen, rechts wird in
   einer Liste von Listen von Listen an jedes Element (d.h. jede Liste von
   Listen) vorne die Leere Liste als neues erstes Element angehaengt.

	map (: []) == \ xs -> map (\ x -> x : []) xs
	map ([] :) == \ xs -> map (\ x -> [] : x) xs

4. Links ist die Identitaet auf Funktionen welche genau zwei Argumente annehmen,
   rechts ist die allgemeine Identitaet.

	flip . flip == \ f x y -> f x y
	id          == \ x -> x

5. Die linke Liste enthaelt genau 3 Elemente, die rechte mindestens 2 und z ist
   hier eine (moeglicherweise leere) Liste und keine Element.

	[x, y, z] == x : y : z : []
	x : y : z == x : y : z
-}

{--------------------------------------- G2 -----------------------------------}

{-

foldl:
 - foldl :: a -> b -> c -> d wegen der Anzahl der Argumente
 - d == b wegen "foldl f z [] = z"
 - c == [e] wegen "[]" und "(x : xs)"
 - a == b -> e -> b wegen "(f z x)"
 - Damit foldl :: (b -> e -> b) -> b -> [e] -> b
 - Kanonisch foldl :: (a -> b -> a) -> a -> [b] -> a

foldr:
 - foldl :: a -> b -> c -> d wegen der Anzahl der Argumente
 - d == b wegen "foldr f z []" == z
 - c == [e] wegen "[]" und "(x : xs)"
 - a == e -> d -> d wegen "f x (foldr f z xs)"
 - Damit foldr :: (e -> d -> d) -> b -> [e] -> d
 - bzw. foldr :: (e -> b -> b) -> b -> [e] -> b
 - Kanonisch: foldr :: (a -> b -> b) -> b -> [a] -> b

isCons:
 - isCons = not . null
 - Mit not :: Bool -> Bool
 - und null :: [a] -> Bool
 - und (.) :: (b -> c) -> (a -> b) -> (a -> c)
 - ist isCons :: [a] -> Bool

ffoldl:
 - Mit foldl :: (a -> b -> a) -> a -> [b] -> a
 - und (.) :: (b -> c) -> (a -> b) -> (a -> c)
 - folgt ffoldl :: (d -> e -> d) -> f -> [g] -> f
 - Aus dem Typen von (.) folgt: d -> [e] -> d == f -> [g] -> f
 - und damit direkt d == f und e == g
 - Also ffoldl :: (d -> e -> d) -> d -> [e] -> d
 - Kanonisch: ffoldl :: (a -> b -> a) -> a -> [b] -> a

oo:
 - Mit (.) :: (b -> c) -> (a -> b) -> (a -> c)
 - da beide Argumente von (.) wieder (.) sind folgt:
 - (b -> c) == (f -> g) -> (e -> f) -> (e -> g)
 - (a -> b) == (i -> j) -> (h -> i) -> (h -> j)
 - Damit b == (f -> g)
 - und c == (e -> f) -> (e -> g)
 - und a == (i -> j)
 - und b == (h -> i) -> (h -> j)
 - Damit f == (h -> i)ffoldl :: (a -> b -> a) -> a -> [[b]] -> a
 - und g == (h -> j)
 - Der Typ von (.) . (.) ist (siehe oben) a -> c
 - Einsetzen liefert
 - a -> c == (i -> j) -> (e -> f) -> (e -> g) ==
 - (i -> j) -> (e -> h -> i) -> (e -> h -> j)
 - Kanonisch: (b -> c) -> (d -> a -> b) -> d -> a -> c

-}

{- ffoldl kann benutzt werden um ueber verschachtelte Listen zu laufen: -}
sumListOfLists xs = ffoldl (+) 0 xs where
	ffoldl = foldl . foldl

-- "sumListOfLists [[1,2,3],[4,5,6],[7,8,9]]" ergibt "45"

{- oo kann benutzt werden um Funktionen zu verketten, bei denen die zuerst
   angewandte Funktion zwei Argumente annimmt: -}
addAndSquare = (^ 2) `oo` (+) where
	oo = (.) . (.)

-- "addAndSquare 5 6" ergibt "121"

{--------------------------------------- G3 -----------------------------------}

{-
Lemma: sum . filter (/= 0) = sum

Proof by extension and definition of (.)

sum . filter (/= 0) = sum
== (sum . filter (/= 0)) xs = sum xs             -- by extension
== sum (filter (/= 0) xs) = sum xs               -- by compose_def

Lemma': sum (filter (/= 0) xs) == sum xs

Proof by structural induction over xs

Base case:
To show: sum (filter (/= 0) []) == sum []

sum (filter (/= 0) [])
== sum []                                        -- by filter_Nil

Induction step:
IH:      sum (filter (/= 0) ys) == sum ys
To show: sum (filter (/= 0) (y : ys)) == sum (y : ys)

Case over ((/= 0) y):

1) True

sum (filter (/= 0) (y : ys)
== sum (y : filter (/= 0) ys)                    -- by filter_Cons
== y + sum (filter (/= 0) ys)                    -- by sum_Cons
== y + sum ys ==                                 -- by IH
== sum (y : ys)                                  -- by sum_Cons (read backwards)

2) False

sum (filter (/= 0) (y : ys)
== sum (filter (/= 0) ys)                        -- by filter_Cons
== sum ys                                        -- by IH
== 0 + sum ys                                    -- by Arithmetic
== y + sum ys                                    -- by ((/= 0) y) == False <=> y == 0
== sum (y : ys)                                  -- by sum_Cons (read backwards)

-}

{--------------------------------------- G4 -----------------------------------}

partition_rec :: (a -> Bool) -> [a] -> ([a], [a])
partition_rec f [] = ([], [])
partition_rec f (x : xs) = if f x then (x : ts, fs) else (ts, x : fs) where
	(ts, fs) = partition_rec f xs

prop_partition_rec :: [Int] -> Bool
prop_partition_rec xs = partition_rec even xs == partition even xs

partition_foldr :: (a -> Bool) -> [a] -> ([a], [a])
partition_foldr f xs = foldr go ([], []) xs where
	go x (ts, fs) = if f x then (x : ts, fs) else (ts, x : fs)

prop_partition_foldr :: [Int] -> Bool
prop_partition_foldr xs = partition_foldr even xs == partition even xs

partition_foldl :: (a -> Bool) -> [a] -> ([a], [a])
partition_foldl f xs = foldl go ([], []) xs where
	go (ts, fs) x = if f x then (ts ++ [x], fs) else (ts, fs ++ [x])

prop_partition_foldl :: [Int] -> Bool
prop_partition_foldl xs = partition_foldl even xs == partition even xs
