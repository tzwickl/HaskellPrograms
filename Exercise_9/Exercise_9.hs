module Exercise_9 where
import Form
import Test.QuickCheck
import Control.Monad
import Data.List as List
import qualified Data.HashMap.Lazy as H
import Data.Hashable

{- Library -- nicht veraendern! -}

data Tree a =
  Empty |
  Node a (Tree a) (Tree a)
    deriving (Eq, Show)

-- allow QuickCheck to generate arbitrary values of type Tree
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized tree
    where
    tree 0 = return Empty
    tree n | n > 0 = oneof [return Empty,
      liftM3 Node arbitrary (tree (n `div` 2)) (tree (n `div` 2))]


{- H9.1 -}

{- 
-------------------------------------givenFunctions---------------------------------------
data Tree a = Empty | Node a ( Tree a ) ( Tree a )			-- tree

inorder Empty = []											-- inorder_Nil
inorder ( Node a l r ) = inorder l ++ [ a ] ++ inorder r	-- inorder_Cons

niorder Empty = []											-- niorder_Nil
niorder ( Node a l r ) = niorder r ++ [ a ] ++ niorder l	-- niorder_Cons

reverse [] = []												-- reverse_Nil
reverse ( x : xs ) = reverse xs ++ [ x ]					-- reverse_Cons
-- Lemmas
( xs ++ ys ) ++ zs = xs ++ ( ys ++ zs ) 					-- append_assoc
reverse ( xs ++ ys ) = reverse ys ++ reverse xs 			-- reverse_append

-----------------------------------------Proof--------------------------------------------
Lemma NieInOrdnung: inorder t = (reverse . niorder) t
Proof by structural induction on t

Base case
To show: inorder (Empty) = (reverse . niorder) (Empty)

inorder (Empty)
= []								-- by definition of inorder_Nil

(reverse . niorder) (Empty)
= reverse (niorder (Empty))			-- by definition of composition-Operator (.)
= reverse []						-- by definition of niorder_Nil
= []								-- by definition of reverse_Nil

Induction step
To show: inorder (Node x t1 t2) = (reverse . niorder) (Node x t1 t2)
Induction-Hypothesis (IH1): inorder (t1) = (reverse . niorder) (t1)
Induction-Hypothesis (IH2): inorder (t2) = (reverse . niorder) (t2)

inorder (Node x t1 t2)
= inorder t1 ++ [x] ++ inorder t2								-- by definition of inorder_Cons
= (reverse . niorder) (t1) ++ [x] ++ inorder t2					-- by IH1
= (reverse . niorder) (t1) ++ [x] ++ (reverse . niorder) (t2)	-- by IH2
= reverse (niorder (t1)) ++ [x] ++ reverse (niorder (t2))		-- by definition of composition-Operator (.)

(reverse . niorder) (Node x t1 t2)
= reverse (niorder (Node x t1 t2))								-- by definition of composition-Operator (.)
= reverse (niorder (t2) ++ [x] ++ niorder (t1))					-- by definition of niorder_Cons
= reverse (niorder (t2) ++ ([x] ++ niorder (t1)))				-- by Lemma append_assoc
= reverse ([x] ++ niorder (t1)) ++ reverse (niorder (t2))		-- by Lemma reverse_append
= reverse (niorder (t1)) ++ [x] ++ reverse (niorder (t2))		-- by reverse_Cons
-}


{- H9.2 -}

{- Teilaufgabe 1 -}
-- checks if a passed boolean formula is in disjunctive normal form
isDnf :: Form -> Bool
isDnf (a :|: b) = isDnf a && isCoclause b
isDnf p 		= isLiteral p || isCoclause p

-- checks if a passed boolean formula is an atom
-- atom is defined as following: T, F and (Var _) are atoms
isAtom :: Form -> Bool
isAtom (F) 				= True
isAtom (T) 				= True
isAtom (Var _) 			= True
isAtom p 				= False

-- checks if a passed boolean formula is a literal
-- literal is defined as following: a literal is an atom or its negation
isLiteral :: Form -> Bool
isLiteral (Not x) 	= isAtom x
isLiteral p 		= isAtom p

-- checks if a passed boolean formula is a co-clause
-- co-clause is defined as following: a boolean formula is in co-clause form if it
-- is in canonical conjunctive normal form
isCoclause :: Form -> Bool
isCoclause (a :&: b) = isCoclause a && isLiteral b
isCoclause p = isLiteral p

{- Teilaufgabe 2 -}
-- converts a passed boolean formula, so that all negations are directly in front of
-- an atom and nowhere else
pushNot :: Form -> Form
pushNot (Not p) 	= pushNot' (pushNot p) where
	pushNot' (Not p)     =  p
	pushNot' (p :&: q)   =  pushNot' p :|: pushNot' q
	pushNot' (p :|: q)   =  pushNot' p :&: pushNot' q
	pushNot' (T)		 =  F
	pushNot' (F) 	 	 =  T
	pushNot' p           =  Not p
pushNot (a :|: b)	= pushNot a :|: pushNot b
pushNot (a :&: b)   = pushNot a :&: pushNot b
pushNot p			= p

{- Teilaufgabe 3 -}
-- converts all boolean formulas of the form a & (b | c) to the form of (a & b) | (a & c) 
-- and all (a | b) & c to the form of (a & c) | (b & c)
pushAnd :: Form -> Form
pushAnd ((a :|: b) :&: c) 	= pushAnd (a :&: c) :|: pushAnd (b :&: c)
pushAnd (a :&: (b :|: c)) 	= pushAnd (a :&: b) :|: pushAnd (a :&: c)
pushAnd (Not p) 			= Not (pushAnd p)
pushAnd (a :|: b)			= pushAnd a :|: pushAnd b
pushAnd (a :&: b) 			= pushAnd a :&: pushAnd b
pushAnd p					= p


{- Teilaufgabe 4 -}
-- converts all boolean formulas of the form (a | (b | c)) to the form of ((a | b) | c)
-- and all (a & (b & c)) to the form of ((a & b) & c)
balance :: Form -> Form
balance (a :|: (b :|: c)) 	= balance (a :|: b) :|: balance c
balance (a :&: (b :&: c))   = balance (a :&: b) :&: balance c
balance (Not p) 			= Not (pushAnd p)
balance (a :|: b)			= balance a :|: balance b
balance (a :&: b) 			= balance a :&: balance b
balance p					= p

{- Teilaufgabe 5 -}
-- converts a passed boolean formula in disjunctive normal form
toDnf :: Form -> Form
toDnf p = fixpoint (==) (balance . pushAnd . pushNot) p where
	fixpoint eq f x = if (eq) x (f x) then x else fixpoint eq f (f x)
	
{- Teilaufgabe 6 -}
-- boolean formulas converted with toDnf must always be evaluated to true with the function
-- isDnf
prop_toDnf1 :: Form -> Bool
prop_toDnf1 p = (isDnf . toDnf) p

-- the result of the evaluation of the boolean formula mustn't change after it is converted in 
-- disjunctive normal form (DNF)
prop_toDnf2 :: Form -> Bool
prop_toDnf2 p = [eval e p | e <- vals(vars p)] == [eval e p' | e <- vals(vars p')]
	where p' = toDnf p

{- H9.3 -}

{- Teilaufgabe 1 -}
-- traverses through the tree and adds up all Nodes
treeSum :: Tree Int -> Int
treeSum (Empty) = 0
treeSum (Node x l r) = x + treeSum l + treeSum r

-- traverses through the tree and compares the nodes with each other and the passed argument
-- returns the minimum element in the tree
treeMin :: Ord a => a -> Tree a -> a
treeMin a (Empty) = a
treeMin a (Node x l r) 
	| a < x = min (treeMin a l) (treeMin a r)
	| otherwise = min (treeMin x l) (treeMin x r) where
	min a b 
		| a < b = a
		| otherwise = b

-- returns a list with all even numbers in the tree
evens :: Tree Int -> [Int]
evens (Empty) = []
evens (Node x l r) 
	| x `mod` 2 == 0 = [x] ++ evens l ++ evens r
	| otherwise = evens l ++ evens r
	
{- Teilaufgabe 2 -}
-- recursive function which works identical as foldl on lists but this time on a tree
-- traverses through a tree and applies the second function on the left and right tree
-- and the first function on the Node and the result of the second function
foldMap :: (a -> b -> b) -> (b -> b -> b) -> b -> Tree a -> b
foldMap f g a (Empty) = a
foldMap f g a (Node x l r) = f x (g (foldMap f g a l) (foldMap f g a r))

-- traverses through the tree and adds up all Nodes
treeSum' :: Tree Int -> Int
treeSum' t = foldMap (+) (+) 0 t

-- traverses through the tree and compares the nodes with each other and the passed argument
-- returns the minimum element in the tree
treeMin' :: Ord a => a -> Tree a -> a
treeMin' a t = foldMap (\x y -> if x < y then x else y) (\x y -> if x < y then x else y) a t

-- returns a list with all even numbers in the tree
evens' :: Tree Int -> [Int]
evens' t = foldMap (\x ys -> if x `mod` 2 == 0 then [x] ++ ys else ys) (++) [] t

{- H9.4 -}

{-WETT-}
-- the function quasiMajorityElemens gets as first the list with the greatest frequency 
-- out of the result list of the function getQuasiMajorityElemns and calculates afterwards 
-- if these elements are really quasi majority elements or not
-- quasi majority elements are elements that have a frequency greater or equal to 50% 
-- if the elements are quasi majority elements the list with the elements is returned else
-- an empty list is returned
quasiMajorityElems :: Eq a => [a] -> [a]
quasiMajorityElems [] = []
quasiMajorityElems xs = if (fromIntegral f / fromIntegral size) >= 0.5 then as else [] where
	(f, as) = getMax r
	(size, r) = getQuasiMajorityElems xs [] []
	getMax (x:lT) = foldl (\(f, xs) (f', ys) -> if f > f' then (f, xs) else (f', ys)) x lT

-- getQuasiMajorityElems iterates through the list of elements and adds each element
-- as first to the first list which consists of all elements with their frequency and as second
-- to the second list which consists of all frequencies and a list with all elements
-- which have the same frequency
-- after execution this function returns a tupel with the number of elements in the 
-- original list as first element, and a List of tuples with all frequencies and a List 
-- with elements which have the same frequency as a second element
getQuasiMajorityElems :: Eq a => [a] -> [(a, Int)] -> [(Int, [a])] -> (Int, [(Int, [a])])
getQuasiMajorityElems [] _ lT2 = (0, lT2)
getQuasiMajorityElems (x:xs) lT1 lT2 = (size + 1, r) where
	(size, r) = getQuasiMajorityElems xs lT1' lT2'
	lT2' = addWordToList (x, f) lT2
	(f, lT1') = lookupFrequency x lT1

-- lookupFrequency iterates through the first List in search of the element a
-- if the element is already in the list the frequency of this element is incremented 
-- by one and the new frequency of the word and the updated list is returned
-- if the element isn't in the list the new element is added to the end of the list with a 
-- frequency of 1
lookupFrequency :: Eq a => a -> [(a, Int)] -> (Int, [(a, Int)])
lookupFrequency a [] = (1, [(a, 1)])
lookupFrequency a ((x, f):xs) 
						| a == x = ((f + 1), (x, (f + 1)) : xs)
						| otherwise = (f', (x, f) : r) where
							(f', r) = lookupFrequency a xs
	
-- addWordToList iterates through the second list in search of the frequency of the current word
-- if the frequency of this word is already in the list the new word is added to the list
-- containing all the words with the same frequency as the new word
-- if the frequency of this word isn't in the list a new tupel is added to the
-- list with the new frequency and the new element as the first element in the list
addWordToList :: Eq a => (a, Int) -> [(Int, [a])] -> [(Int, [a])]
addWordToList (a, f) [] = [(f, [a])]
addWordToList (a, f) ((f', as):xs) 
						| f == f' = (f', a : as) : xs
						| otherwise = (f', as) : r where
							r = addWordToList (a, f) xs
{-TTEW-}