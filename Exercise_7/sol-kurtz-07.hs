import Data.Ratio
import Test.QuickCheck

{----------------------------------- G1 ---------------------------------------}

data Fraction = Integer `Over` Integer
-- ebenso moeglich: data Fraction = Over Integer Integer

canonicalizeFraction :: Fraction -> Fraction
canonicalizeFraction (x `Over` y) =
	(x `div` z) `Over` (y `div` z) where z = gcd x y * signum y
-- ebenso moeglich: canonicalizeFraction (Over x y) = ...

{- Optionale aber hilfreiche Implementierung der "Show"-Klasse:

class Show a where
	show :: a -> String

-}
instance Show Fraction where
	show x = show y ++ "/" ++ show z where
		(y `Over` z) = canonicalizeFraction x

instance Num Fraction where
	(a `Over` b) + (c `Over` d) =
		canonicalizeFraction ((a * d + c * b) `Over` (b * d))
	(a `Over` b) - (c `Over` d) =
		canonicalizeFraction ((a * d - c * b) `Over` (b * d))
	(a `Over` b) * (c `Over` d) =
		canonicalizeFraction ((a * c) `Over` (b * d))
	negate (a `Over` b)         = canonicalizeFraction ((- a) `Over` b)
	fromInteger a               = a `Over` 1
	abs (a `Over` b)            = canonicalizeFraction (abs a `Over` abs b)
	signum (a `Over` b)         = fromInteger (signum a * signum b)

instance Eq Fraction where
	(a `Over` b) == (c `Over` d) =
		a * d == b * c

	-- Alternative:
	-- x == y = a == c && b == d where
	-- 	(a `Over` b) = canonicalizeFraction x
	-- 	(c `Over` d) = canonicalizeFraction y

-- Optional; nur fuer die Tests notwendig. War *nicht* in der Vorlesung dran.
instance Arbitrary Fraction where
	arbitrary = do
		x <- arbitrarySizedIntegral
		y <- arbitrarySizedIntegral
		if y == 0 then
			return (x `Over` 1)
		else
			return (x `Over` y)
	shrink (x `Over` y) = do
		x' <- shrinkIntegral x
		y' <- shrinkIntegral y
		return (x' `Over` y')

prop_signum :: Fraction -> Bool
prop_signum x = abs x * signum x == x

prop_negate1 :: Fraction -> Bool
prop_negate1 x = negate x == (fromInteger 0) - x

prop_negate2 :: Fraction -> Bool
prop_negate2 x = negate x + x == (fromInteger 0)

instance Fractional Fraction where
	(a `Over` b) / (c `Over` d) =
		canonicalizeFraction ((a * d) `Over` (b * c))
	fromRational x              =
		numerator x `Over` denominator x

{----------------------------------- G2 ---------------------------------------}

f1 xs = map (\x -> x + 1) xs
f2 xs = map (\x -> 2 * x) (map (\x -> x + 1) xs)
f3 xs = filter (\x -> x > 1) (map (\x -> x + 1) xs)
f4 f g x = f (g x)
f5 f g x y = f (g x y)
f6 f g x y z = f (g x y z)
f7 f g h x = g (h (f x))

f1' = map (+1)

f2' = map (2*) . map (+1)
f2'' = map ((2*) . (+1))

f3' = filter (>1) . map (+1)

f4' f g = f . g
f4'' f = (.) f
f4''' = (.)

f5' f g x = f . g x
f5'' f g = ((.).(.)) f g
f5''' f = ((.).(.)) f
f5'''' = (.).(.)  -- vgl. "oo"

f6' f g x y = f . g x y
f6'' f g x = ((.).(.)) f . g
f6''' f g = ((.).(.).(.)) f g
f6'''' f = ((.).(.).(.)) f
f6''''' = (.).(.).(.)

f7' f g h = g . h . f

{----------------------------------- G3 ---------------------------------------}

data Shape = Circle Integer
	| Rectangle Integer Integer
	| Triangle Integer Integer Integer
	deriving (Eq, Show)

isValid :: Shape -> Bool
isValid (Circle r)       = r >= 0
isValid (Rectangle l w)  = l >= 0 && w >= 0
isValid (Triangle a b c) = a >= 0 && b >= 0 && c >= 0 &&
	a + b >= c && a + c >= b && b + c >= a

scale :: Integer -> Shape -> Shape
scale x (Circle r)       = Circle (r * x)
scale x (Rectangle l w)  = Rectangle (l * x) (w * x)
scale x (Triangle a b c) = Triangle (a * x) (b * x) (c * x)
