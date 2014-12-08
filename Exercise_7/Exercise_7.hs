module Exercise_7 where
import Data.Maybe
import Data.HashMap.Lazy
import Data.List
import Test.QuickCheck

{- Template NUR fuer die Hausaufgaben! -}

{- Library -- nicht veraendern --}

data Shape =
  Circle Integer |
  Rectangle Integer Integer
  deriving (Show, Eq)

{- Ende Library -}

{- H1 -}
{- Teilaufgabe 1 -}
-- calculates the area of a shape
area :: Shape -> Maybe Integer
area (Circle r) = Nothing
area (Rectangle l w) = Just (l * w)


{- Teilaufgabe 2 -}
-- calculates the sum of the areas of several rectangles
rectArea :: [Shape] -> Integer
rectArea [] = 0
rectArea (x:xs) = if isNothing (area x) then rectArea xs else fromJust (area x) + rectArea xs

{- Teilaufgabe 3 -}
-- x coordinate in a cartesian coordinate system
type X = Integer
-- y coordinate in a cartesian coordinate system
type Y = Integer
-- shape in the coordinate system
type S = Shape

-- new datatype that additionally to a shape also its coordinates consists
data PosShape = 
	Shape S X Y
	deriving (Show, Eq)

{- Teilaufgabe 4 -}
-- scales a shape accordingly to a scale factor without affecting the position
-- of the shape in the coordinate system
scale :: Integer -> PosShape -> PosShape
scale sf (Shape (Circle r) x y) = Shape (Circle (r*sf)) x y
scale sf (Shape (Rectangle l w) x y) = Shape (Rectangle (l*sf) (w*sf)) x y

{- Teilaufgabe 5 -}
-- moves a shape along the x and the y axis with a factor a and b respectively
move :: (Integer, Integer) -> PosShape -> PosShape
move (a, b) (Shape s x y) = Shape s (x+a) (y+b)

{- H2 -}
{- Teilaufgabe 1 -}
-- defines a new datatype that can either saves a color in the RGB color space or in the
-- YUV color space
data Colour = 
	RGB Float Float Float |
	YUV Float Float Float
	deriving (Show, Eq)
	
-- checks if the colours are valid
isColourValid :: Colour -> Bool
isColourValid (RGB r g b) = and (Prelude.map (\x -> x >= 0 && x <= 1) [r, g, b])
isColourValid (YUV y u v) = (y >= 0 && y <= 1) && (u >= (-0.436) && u <= 0.436) &&
	(v >= (-0.615) && v <= 0.615)
	
{- Teilaufgabe 2 -}
-- returns the red component of a colour
redComponent :: Colour -> Float
redComponent (RGB r _ _) = r
redComponent (YUV y _ v) = if r < 0 then 0 else if r > 1 then 1 else r where
	r = y + v * 1.139837398

-- returns the green component of a colour
greenComponent :: Colour -> Float
greenComponent (RGB _ g _) = g
greenComponent (YUV y u v) = if g < 0 then 0 else if g > 1 then 1 else g where
	g = y - u * 0.3946517044 - v * 0.5805986067

-- returns the blue component of a colour
blueComponent :: Colour -> Float
blueComponent (RGB _ _ b) = b 
blueComponent (YUV y u _) = if b < 0 then 0 else if b > 1 then 1 else b where
	b = y + u * 2.032110092


{- H3 -}

{-WETT-}
fixTypos :: [String] -> [String] -> [String]
fixTypos vocabs words = compareWandV words (addToHashMap vocabs empty)

-- inserts a list of Strings into a HashMap and uses as a key the length of the Strings
-- map Strings according to their length
addToHashMap :: [String] -> HashMap Int [String] -> HashMap Int [String]
addToHashMap [] hm = hm
addToHashMap (x:xs) hm = addToHashMap xs (Data.HashMap.Lazy.insertWith 
	(\xs ys -> if elem (concat xs) ys then ys else ys++xs) (length x) [x] hm) where

-- compares all words with a list of vocabularies and substitutes the wrong word in the 
-- list if a quasi-identical word could be found the the vocabulary
compareWandV :: [String] -> HashMap Int [String] -> [String]
compareWandV [] _ = []
compareWandV (w:words) hm = if s == "" then w : compareWandV words hm
	else s : compareWandV words hm where
	s = if lookupV == Nothing then "" else compareStrings w (fromJust lookupV) []
	lookupV = (Data.HashMap.Lazy.lookup (length w) hm) 

-- compares a String with a list of Strings and checks whether on of the Strings in the
-- list are quasi-identical with the String or not
-- if more than one String is quasi-identical or none an empty String is returned, else
-- the quasi-identical String is returned
compareStrings :: String -> [String] -> [String] -> String
compareStrings _ [] [] = ""
compareStrings _ [] (p:[]) = p
compareStrings _ [] (p:puf) = ""
compareStrings word (v:vocabs) puf = if isQuasiIdentical word v 
	then compareStrings word vocabs (v:puf) else compareStrings word vocabs puf

-- compares two strings and decides if they are quasi-identical or not
-- quasi-identical is defined as: they must have the same length and only one letter is 
-- allowed to be different
isQuasiIdentical :: String -> String -> Bool
isQuasiIdentical xs ys = go xs ys 0 where
	go _ _ 2 = False
	go [] [] f = True
	go (x:xs) (y:ys) f = if x == y then go xs ys f else go xs ys (f+1)
	
-- input List must have the same length as the output List
prop_fixTypos :: [String] -> [String] -> Property
prop_fixTypos as bs = length as > 0 && length bs > 0 && not(elem "" as) && not(elem "" bs)
	==> length (fixTypos as bs) == length bs
	
-- the length of each element in the output list must be equal with the length of each 
-- element in the input list
prop_fixTypos' :: [String] -> [String] -> Property
prop_fixTypos' as bs = length as > 0 && length bs > 0 && not(elem "" as) && not(elem "" bs)
	==> [length b|b <- bs] == [length o| o <- fixTypos as bs]

{-TTEW-}