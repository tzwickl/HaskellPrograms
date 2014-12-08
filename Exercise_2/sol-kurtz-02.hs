import Test.QuickCheck

all_sums :: [Integer] -> [Integer] -> [Integer]
all_sums xs ys = [ x+y | x <- xs, y <- ys ]

prop_all_sums_length :: [Integer] -> [Integer] -> Bool
prop_all_sums_length xs ys = length (all_sums xs ys) == length xs * length ys

prop_all_sums_max xs ys = null xs || null ys ||
	maximum (all_sums xs ys) == maximum xs + maximum ys

evens :: [Integer] -> [Integer]
evens xs = [ x | x <- xs, x `mod` 2 == 0 ]

n_lists :: [Integer] -> [[Integer]]
n_lists xs = [ [1..x] | x <- xs ]

all_even_sum_lists :: [Integer] -> [Integer] -> [[Integer]]
all_even_sum_lists xs ys =
	[ [1..(x+y)] | x <- xs, y <- ys, (x + y) `mod` 2 == 0 ]

prop_all_even_sum_lists :: [Integer] -> [Integer] -> Bool
prop_all_even_sum_lists xs ys = all_even_sum_lists xs ys ==
	n_lists (evens (all_sums xs ys))

union :: [Integer] -> [Integer] -> [Integer]
union xs ys = xs ++ ys

union' :: [Integer] -> [Integer] -> [Integer]
union' xs ys = xs ++ diff ys xs

intersection :: [Integer] -> [Integer] -> [Integer]
intersection xs ys = [ x | x <- xs, x `elem` ys ]

intersection' :: [Integer] -> [Integer] -> [Integer]
intersection' xs ys = [ x | x <- xs, y <- ys, x == y ]

diff :: [Integer] -> [Integer] -> [Integer]
diff xs ys = [ x | x <- xs, not (x `elem` ys) ]

prop_intersection :: Integer -> [Integer] -> [Integer] -> Bool
prop_intersection x xs ys =
	(x `elem` (intersection xs ys)) == (x `elem` xs && x `elem` ys)

elem' :: Integer -> [Integer] -> Bool
elem' x xs = not (null [y | y <- xs , y == x ])

eq_frac :: (Integer , Integer) -> (Integer , Integer) -> Bool
eq_frac (a, b) (c, d) = a * d == b * c

prop_eq_frac1 :: (Integer, Integer) -> (Integer, Integer) -> Bool
prop_eq_frac1 x y = eq_frac x y == eq_frac y x

prop_eq_frac2 :: (Integer, Integer) -> Integer -> Bool
prop_eq_frac2 (m, n) k = eq_frac (m, n) (m * k, n * k)

pow2 :: Integer -> Integer
pow2 0 = 1
pow2 n | n > 0 = 2 * pow2 ( n - 1)

pow2' 0 = 1
pow2' 1 = 2
pow2' n = k * k * pow2' (n `mod` 2) where k = pow2' (n `div` 2)
