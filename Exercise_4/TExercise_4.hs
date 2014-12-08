module TExercise_4 where
import Data.List

{----------------------------------------------------------------------------------------}
{- G4.1 -}

-- implements a function which checks a list if it has the property of a fibonacci list
hasFibonacciProperty :: [Integer] -> Bool
hasFibonacciProperty [] = True
hasFibonacciProperty (y:ys) = hasFibonacciPropertyAux ys y

hasFibonacciPropertyAux :: [Integer] -> Integer -> Bool
hasFibonacciPropertyAux [_] _ = True
hasFibonacciPropertyAux (x:(y:xs)) z = z + x == y && hasFibonacciPropertyAux (y:xs) x

{----------------------------------------------------------------------------------------}
{- G4.2 -}
{- 
Lemma rev_snoc: reverse ( snoc xs x ) = x : reverse xs
Proof by structural induction on xs

Base case
Show reverse ( snoc [] x ) = x : reverse []

reverse ( snoc [] x )
= reverse ( [x] )					-- by definition of snoc_Nil
= reverse [] ++ [x]					-- by definition of reverse_Cons
= [] ++ [x]							-- by definition of reverse_Nil
= [x]								-- by definition of append_Nil

x : reverse []
= x : []							-- by definition of reverse_Nil
= [x]

Induction step
Show: reverse ( snoc (y:xs) x ) = x : reverse (y:xs)
Induction-hypothesis (IH): reverse ( snoc xs x ) = x : reverse xs

reverse ( snoc (y:xs) x )
= reverse ( y : snoc xs x )			-- by definition of snoc_Cons
= reverse (snoc xs x) ++ [y]		-- by definition of reverse_Cons
= (x : reverse xs) ++ [y]			-- by IH
= x : (reverse xs ++ [y])			-- by append_Cons

x : reverse [y:xs]
= x : (reverse xs ++ [y])			-- by definition of reverse_Cons
 -}

{----------------------------------------------------------------------------------------}
{- G4.3 -}
-- defines a function which returns all possible permutation of a word
perms :: [Char] -> [[Char]]
perms [] = [[]]
perms xs = reverse $ sort $ nub [x:ys| x <- xs, ys <- perms (delete x xs)]

{----------------------------------------------------------------------------------------}
{- G4.3 -}
-- defines a function which applies a pattern on a string
match :: [Char] -> [Char] -> Bool
match [] bs             = null bs
match ('?':as) (_:bs)   = match as bs
match ('*':as) []       = match as []
match ('*':as) (b:bs)   = match as (b:bs) || match ('*':as) bs
match (a:as)   []       = False
match (a:as)   (b:bs)   = a == b && match as bs
