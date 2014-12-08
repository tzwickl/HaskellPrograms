module Exercise_4 where
import Test.QuickCheck
import Data.List

{- G1 -}
-- checks a list if it fulfils the fibonacci properties
hasFibonacciProperty :: [Integer] -> Bool
hasFibonacciProperty [] = True
hasFibonacciProperty [_] = True
hasFibonacciProperty [_,_] = True
hasFibonacciProperty (x:y:z:xs) = x + y == z && hasFibonacciProperty (y:(z:xs))

{- G2 -}

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
= x : (reverse xs ++ [y])				-- by definition of reverse_Cons
 -}

{- G3 -}
-- generates a list with all possible permutations of a word without duplicates and
-- in ASCII order
perms :: [Char] -> [[Char]]
perms [] = [[]]
perms xs = reverse $ sort $ nub [x:ys| x <- xs, ys <- perms (delete x xs)]

{- G4 -}

match :: [Char] -> [Char] -> Bool
match [] bs             = null bs
match ('?':as) (_:bs)   = match as bs
match ('*':as) []       = match as []
match ('*':as) (b:bs)   = match as (b:bs) || match ('*':as) bs
match (a:as)   []       = False
match (a:as)   (b:bs)   = a == b && match as bs

{- H1 -}

matchingWords :: [String] -> Bool
matchingWords [] = True
matchingWords [_] = True
matchingWords (w:x:ws) = if null w then matchingWords (x:ws)
	else if null x then matchingWords (w:ws) 
	else last w == head x && matchingWords (x:ws)

{- H2 -}

chunks :: [Int] -> [a] -> [[a]]
chunks [] _ = []
chunks (x:xs) as = [split x as] ++ chunks xs (rem x as) where
	split 0 as = []
	split x (a:as) = a : split (x-1) as
	rem 0 as = as
	rem x (a:as) = rem (x-1) as

-- QuickCheck-Tests
-- verboseCheckWith stdArgs {maxDiscardRatio=1000,maxSize=5} you can set the numbers
-- of discarded tries before quickCheck gives up
-- as more preconditions you set as harder it is for quickCheck to find suitable values

-- length of the sublists from the function chunks must be equal with the length of xs
prop_chunks1 :: [Int] -> String -> Property
prop_chunks1 xs as = not (null xs) && sum xs <= length as && and [x > 0| x <- xs] ==> 
	length (chunks xs as) == length xs

-- [k0, k1, k2, ..., kn] [x0, x1, x2, ..., xn]
-- sublist x0 must have the length k0, sublist x1 must have the length k1 
-- and the sublist xn must have the length kn
prop_chunks2 :: [Int] -> String -> Property
prop_chunks2 xs as = not (null xs) && sum xs <= length as && and [x > 0| x <- xs] ==> 
	compLen (chunks xs as) xs where
		compLen [] [] = True
		compLen (a:as) (x:xs) = length a == x && compLen as xs
		
-- all sublists from the function chunks concatenated must be equal with the first part
-- of the input string as (remaining letters in as can be ignored)
prop_chunks3 :: [Int] -> String -> Property
prop_chunks3 xs as = not (null xs) && sum xs <= length as && and [x > 0| x <- xs] ==> 
	compEq (concat (chunks xs as)) as where
		compEq [] _ = True
		compEq (x:xs) (a:as) = x == a && compEq xs as

{- H3 -}

{- 
Lemma ängeL: length ( snoc xs x ) = length ( x : xs )
Proof by structural induction on xs

Base case
Show length ( snoc [] x ) = length ( x : [] )

length ( snoc [] x )
= length ( [x] )					-- by definition of snoc_Nil
= length [] + 1						-- by definition of length_Cons
= 0 + 1								-- by definition of length_Nil
= 1

length x : []
= length [] + 1						-- by definition of length_Cons
= 0 + 1								-- by definition of length_Nil
= 1

Induction step
Show: length ( snoc (y : xs) x ) = length ( x : (y : xs) )
Induction-hypothesis (IH): length ( snoc xs x ) = length ( x : xs )

length ( snoc (y : xs) x )
= length ( y : snoc xs x )			-- by definition of snoc_Cons
= length ( snoc xs x ) + 1			-- by definition of length_Cons
= length ( x : xs ) + 1				-- by IH
= length ( xs ) + 1 + 1				-- by definition of length_Cons
= length ( xs ) + 2

length ( x : (y : xs) )
= length ( (y : xs) ) + 1			-- by definition of length_Cons
= length ( xs ) + 1 + 1				-- by definition of length_Cons
= length ( xs ) + 2
-}

{- H4 -}

{- Abgabe von Aufgabe 4 mit dem nächsten Blatt! -}
