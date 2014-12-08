module TExercise_2 where
import Data.List
import Test.QuickCheck


{- G2.1 -}

{- Teilaufgabe 1 -}
all_sums :: [Integer] -> [Integer] -> [Integer]
all_sums xs ys = [x+y| x <- xs, y <- ys]

{- Teilaufgabe 2 -}
evens :: [Integer] -> [Integer]
evens x = [y| y <- x, (y `mod` 2) == 0]

{- Teilaufgabe 3 -}
n_lists xs = [[1..x]| x <- xs]

-- alternative way of solving
n_lists_alt :: [Integer] -> [[Integer]]
n_lists_alt [] = []
n_lists_alt (x:xs) = [[y| y <- [1..x]]] ++ n_lists xs

-- checks the equality of the two functions n_lists
prop_n_lists :: [Integer] -> Bool
prop_n_lists xs = n_lists xs == n_lists_alt xs

{- Teilaufgabe 4 -}
all_even_sum_lists :: [Integer] -> [Integer] -> [[Integer]]
all_even_sum_lists xs ys = [[1..(x+y)]| x <- xs, y <- ys, ((x+y) `mod` 2) == 0]

prop_all_even_sum_lists :: [Integer] -> [Integer] -> Bool
prop_all_even_sum_lists xs ys = all_even_sum_lists xs ys == n_lists (evens (all_sums xs ys))

{------------------------------------------------------------------}
{- G2.2 -}

{- Teilaufgabe 1 -}
-- calculates the union of two lists (A U B)
union :: [Integer] -> [Integer] -> [Integer]
union xs ys = [x| x <- xs] ++ [y| y <- ys, not(elem y xs)]

{- Teilaufgabe 2 -}
-- calculates the intersection of two lists (A n B)
intersection :: [Integer] -> [Integer] -> [Integer]
intersection xs ys = [x| x <- xs, elem x ys]

{- Teilaufgabe 3 -}
-- calculates the difference of two lists (A \ B)
diff :: [Integer] -> [Integer] -> [Integer]
diff xs ys = [x| x <- xs, not(elem x ys)]

{- Teilaufgabe 4 -}
-- Test property for the intestection method
-- with verboseCheckWith (stdArgs {maxSize=n}) you can set the maximal size of the two list parameters
prop_intersection :: Integer -> [Integer] -> [Integer] -> Bool
prop_intersection x xs ys = (elem x xs && elem x ys) == elem x (intersection xs ys)

{- Teilaufgabe 5 -}
-- implements an own version of the elem :: Integer -> [Integer] -> Bool function
elem' :: Integer -> [Integer] -> Bool
elem' x ys = not (null [y| y <- ys, y == x])

-- Test property for the elem_own function
prop_elem :: Integer -> [Integer] -> Bool
prop_elem x xs = elem' x xs == elem x xs

{------------------------------------------------------------------}
{- G2.3 -}

{- Teilaufgabe 1 -}
-- checks if the two fractions are equal in value
eq_frac :: (Integer, Integer) -> (Integer, Integer) -> Bool
eq_frac (x1,y1) (x2,y2) = (y1*x2) == (y2*x1)

{- Teilaufgabe 2 -}
-- checks if the eq_frac function works porperly
-- check symmetry
prop_eq_frac1 :: (Integer, Integer) -> (Integer, Integer) -> Bool
prop_eq_frac1 x y = eq_frac x y == eq_frac y x

-- adding a constant mustn't change the equality of a fraction
prop_eq_frac2 :: (Integer, Integer) -> Integer -> Bool
prop_eq_frac2 (x,y) k = eq_frac (x,y) (x*k, y*k)

{------------------------------------------------------------------}
{- G3.3 -}

-- calculates the power of 2
-- usual function which takes n steps for solving
pow2 :: Integer -> Integer
pow2 0 = 1
pow2 n | n >= 1 = 2 * pow2 (n - 1)

-- smart function which takes only O(2 * lb n)
pow2' :: Integer -> Integer
pow2' 0 = 1
pow2' n | n < 0 = undefined
		| (n `mod` 2) == 0 = k * k
		| otherwise = 2 * pow2'(n - 1)
		where k = pow2'(n `div` 2)
		
-- checks if the two pow2 functions are equivalent
prop_pow2 n = 
	n > 0 ==> pow2 n == pow2' n