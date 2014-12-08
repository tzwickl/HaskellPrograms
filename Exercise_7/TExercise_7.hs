module TExercise_7 where
import Test.QuickCheck
import Data.Ratio

{- Aufgabe G7.1 -}
--defines our own data type which takes two integers
--same notation: Integer `Over` Integer
data Fraction = Over Integer Integer

--canonicalize a fraction so it has the smallest possible divisor
canonicalizeFraction :: Fraction -> Fraction
canonicalizeFraction (x `Over` y) =
	(x `div` z) `Over` (y `div` z) where z = gcd x y * signum y

--implements the show function for Fractions
instance Show Fraction where
	show x = show a ++ "/" ++ show b where (a `Over` b) = canonicalizeFraction x
	
--implements arithmetical operation for Fractions
instance Num Fraction where
	(x `Over` y) + (a `Over` b) = canonicalizeFraction ((x * b + a * y) `Over` (y * b))
	(x `Over` y) - (a `Over` b) = canonicalizeFraction ((x * b - a * y) `Over` (y * b))
	(x `Over` y) * (a `Over` b) = canonicalizeFraction ((x * y) `Over` (a * b))
	negate (x `Over` y) = canonicalizeFraction ((-x) `Over` y)
	fromInteger a = a `Over` 1
	abs (x `Over` y) = canonicalizeFraction (abs x `Over` abs y) 
	signum (x `Over` y) = fromInteger (signum x * signum y)
	
--implements the equals methode
instance Eq Fraction where
	(x `Over` y) == (a `Over` b) = x * b == y * a

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
		
{- Aufgabe G7.2 -}

f1 xs = map (\ x -> x + 1) xs
f1' = map (+1)

f2 xs = map (\ x -> 2 * x ) ( map (\ x -> x + 1) xs )
f2' = map (*2) . map (+1)

f3 xs = filter (\ x -> x > 1) ( map (\ x -> x + 1) xs )
f3' = filter (>1) . map (+1)

f4 f g x = f ( g x )
f4' f g x = f . g x

f5 f g x y = f ( g x y )
f5' f g = f . g

f6 f g x y z = f ( g x y z )
f6' f g = f . g

f7 f g h x = g ( h ( f x ))
f7' f g h = g . h . f


{- Aufgabe G7.3 -}
type Radius = Float
type Length = Float
type Width = Float
type Height = Float

data Shape = Circle Radius | Rectangle Length Width | Triangle Length Width Height

--implements the show methods for Radius, Rectangle and Triangle
instance Show Shape where
	show (Circle x) = "Circle with a Radius of " ++ show x
	show (Rectangle x y) = "Rectangle with a Length of " ++ show x ++ " and a Width of " ++ show y
	show (Triangle x y z) = "Triangle with a Length of " ++ show x ++ ", a Width of " ++ show y ++ " and a Height of " ++ show z
	
--checks if all shapes have a valid value for Radius, Height, Length and Width
isValid :: Shape -> Bool
isValid	(Circle x) = x > 0
isValid	(Rectangle x y) = x > 0 && y > 0
isValid	(Triangle x y z) = x > 0 && y > 0 && z > 0	
	
--scales a figure according to a scale factor sf
scale :: Float -> Shape -> Shape
scale sf (Circle x) = Circle (x*sf)
scale sf (Rectangle x y) = Rectangle (x*sf) (y*sf)
scale sf (Triangle x y z) = Triangle (x*sf) (y*sf) (z*sf)