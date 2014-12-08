import Test.QuickCheck

threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent a b c = a /= b && b /= c && a /= c

fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual a b c d = a == b && b == c && c == d

{-

Achtung: Folgende Auswertung ignoriert das Haskell eigentlich lazy ist.

threeDifferent (2+3) 5 (11 ` div ` 2) ==
threeDifferent 5 5 5 ==
5 /= 5 && 5 /= 5 && 5 /= 5 ==
False && False && False ==
False

fourEqual (2+3) 5 (11 ` div ` 2) (21 ` mod ` 11) ==
fourEqual 5 5 5 10 ==
5 == 5 && 5 == 5 && 5 == 10 ==
True && True && False ==
False

-}

fac :: Integer -> Integer
fac n
	| n <= 0    = 1
	| otherwise = n * fac (n - 1)

fac' n =
	if n < 0 then
		1
	else
		if n == 0 then
			1
		else
			n * fac (n - 1)

fac'' 0 = 1
fac'' n = n * fac (n - 1)

sum' :: Integer -> Integer
sum' x = if x <= 0 then 0 else x + sum' (x-1)

sum_eleven :: Integer -> Integer
sum_eleven n = sum' (n + 10) - sum' (n - 1)

sum_eleven' :: Integer -> Integer
sum_eleven' n = 11 * n + sum' 10

prop_sum_eleven :: Integer -> Bool
prop_sum_eleven n = n <= 0 || sum_eleven n == sum_eleven' n

g :: Integer -> Integer
g n = if n < 10 then n * n else n

max_g :: Integer -> Integer
max_g n
	| n <= 0    = 0
	| otherwise = if g n > g x then n else x where
		x = max_g (n - 1)

max_g' :: Integer -> Integer
max_g' n
	| n <= 0 = 0
	| n > 0  = if 10 <= n && n <= 81 then 9 else n

prop_max_g :: Integer -> Bool
prop_max_g n = max_g n == max_g' n
