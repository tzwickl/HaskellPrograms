-- The line "module TExercise_1 where" allows this file to be imported by other Haskell files; it is optional if you only use one Haskell file at a time.
module TExercise_1 where

import Test.QuickCheck

{---------------------------------------------------------------------}
{- Aufgabe G1.1 -}

{- Teil 1 -}

-- defines a function which only returns true if all three paramters are different from each other
threeDifferent :: Integer -> Integer -> Integer -> Bool
-- solved with guarded equations
--threeDifferent x y z
--	| (x == y) && (x == z) && (y == z) = False
--	| otherwise = True
-- alternative way to solve this problem with if-then-else
-- threeDifferent x y z = if (x == y) && (x == z) && (y == z) then False else True
-- simplest way to solve the problem with boolean evaluation
threeDifferent x y z = x /= y && x /= z && y /= z
	
{- Teil 2 -}

-- defines a functino which only returns true if all four paramters are equal to each other
fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual a b c d = (a == b) && (b == c) && (c == d)

{- Teil 3 -}

{- line-wise evaluation of expression "threeDifferent (2+3) 5 (11 `div` 2)"
threeDifferent (2+3) 5 (11 `div` 2) = threeDifferent 5 5 5
									= (5 == 5) && (5 == 5) && (5 == 5)
									= True && True && True
									= False
-}
{- line-wise evaluation of expression "fourEqual (2+3) 5 (11 `div` 2) (21 `mod` 11)"
fourEqual (2+3) 5 (11 `div` 2) (21 `mod` 11) 	= fourEqual 5 5 5 10
												= (threeDifferent 5 5 5) || (threeDifferent 5 5 10)
												= ((5 == 5) && (5 == 5) && (5 == 5)) || ((5 == 5) && (5 == 5) && (5 == 10))
												= (False) || (True)
												= False
-}

	
{---------------------------------------------------------------------}
{- Aufgabe G1.2 -}

{- Teil 1 -}
-- defines a recursive function which calcuates the faculty of a number
fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)

{- Teil 2 -}
-- defines a function which sums the numbers from n to n+10
sum_eleven :: Integer -> Integer
sum_eleven n = sum_aux n 11

-- recursive auxiliary function
-- first parameter is the starting point the second one is number of steps, step range is one
sum_aux :: Integer -> Integer -> Integer
sum_aux n 0 = 0
sum_aux n i = n + sum_aux (n+1) (i-1)

{---------------------------------------------------------------------}
{- Aufgabe G1.3 -}

-- given function
g :: Integer -> Integer
g n = if n < 10 then n*n else n

-- defines a max function which returns the maximum between 0 and n of the given function g
max_g :: Integer -> Integer
max_g 0 = g 0
max_g n 
	| g n > g x = n
	| otherwise = x
		where x = max_g (n - 1)

-- quickCheck test for testing the max_g function
-- confines range to all positive numbers
prop_max n = 
	n >= 0 ==> if n >= 10 && n <= 81 then max_g n == 9 else max_g n == Z
{---------------------------------------------------------------------}
{- additional task -}

-- defines a function which calculates all n fibonacci numbers
fibonacci :: Integer -> [Integer]
fibonacci 0 = [0]
fibonacci n = fibonacci (n - 1) ++ [fib n]

-- defines a function which calculates the nth fibonacci number
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)