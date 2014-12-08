module Exercise_3 where
import Data.List
import Data.Char
import Test.QuickCheck

{- Library DO NOT CHANGE -}
type CentreFun = [Char] -> [Char]
type Picture = [[Char]]

printPicture :: Picture -> IO ()
printPicture [] = return ()
printPicture (xs : xss) = do
  putStrLn xs
  printPicture xss

pic = [".##.", ".#.#", ".###", "####"]
{- End Library -}

{- H1 -}

simplifySpaces :: [Char] -> [Char]
simplifySpaces [] = []
simplifySpaces (x:xs) = cut xs' where
	x' = if isSpace x then ' ' else x
	xs' = substitute (rem x' xs)
	rem x [] = [x]
	rem x (y:ys) = if isSpace x && ((isSpace x) == isSpace y) 
		then rem ' ' ys else x : rem y ys
	substitute xs = [if (isSpace x) && x /= ' ' then ' ' else x | x <- xs]

cut :: [Char] -> [Char]
cut [] = []
cut xs = reverse (cutHead (reverse $ cutHead xs)) where
	cutHead [] = []
	cutHead (x:xs) = if (isSpace x) then cut xs else (x:xs)

{- H2 -}
{- Teilaufgabe 1 -}
-- Anzahl der Zeilen in der Ausgabe muss die gleiche sein wie die in der Eingabe
prop_centre1 :: CentreFun -> [Char] -> Bool
prop_centre1 wrap xs = length (lines (wrap xs)) == length (lines xs)

{- Teilaufgabe 2 -}
-- Länge der Zeilen müssen gleichen sein
prop_centre2 :: CentreFun -> [Char] -> Bool
prop_centre2 wrap xs = and (map (== head xs') (tail xs')) where
	xs' = (map (length) [x| x <- lines (wrap xs)]) ++ (map (length) [y|y <- lines (wrap xs)])

-- Längste Zeile hat keine führenden oder nachfolgenden Leersymbole
-- Es muss mindestens eine Zeile geben die keine führenden oder nachfolgenden Leersymbole
-- hat, aber es können auch mehrere sein
prop_centre3 :: CentreFun -> [Char] -> Bool
prop_centre3 wrap xs = length ([x| x <- lines (wrap xs), not (isSymbol (head x)), 
	not (isSymbol (last x))]) >= 1

{- Teilaufgabe 3 -}
-- Abgesehen von führenden und folgenden Leersymbolen sind Ein- und Ausgabe identisch
prop_centre4 :: CentreFun -> [Char] -> Bool
prop_centre4 wrap xs = [cut x| x <- lines xs] == 
	[cut x| x <- lines (wrap xs)] where

{- Teilaufgabe 4 -}
-- Als führende und folgende Leersymbole einer Zeile sind nur Leerzeichen erlaubt
-- zuerst werden alle führenden und folgenden Leerzeichen jeder Zeile entfernt und dann
-- wird geprüft ob keine weiteren führenden oder folgenden Leersymbole vorhanden sind
prop_centre5 :: CentreFun -> [Char] -> Bool
prop_centre5 wrap xs = 
	and [(not (isSpace (head x))) && (not (isSpace (last x)))| x <- xs', length x > 0] 
	where
	xs' = [reverse (cut' (reverse (cut' (x))))| x <- lines (wrap xs)]
	cut' [] = []
	cut' (x:xs) = if (isSpace x) && x == ' ' then cut xs else (x:xs)

{- Teilaufgabe 5 -}
-- Führende und folgende Leerzeichen dürfen sich um maximal 1 in der Anzahl unterscheiden
-- Zählt zuerst die führenden Leerzeichen, dann die folgenden Leerzeichen und berechnet
-- dann die Differenz aus den zwei Mengen. Danach wird mit filter geprüft ob die 
-- Differenz jeder Zeile nicht größer als 1 ist
prop_centre6 :: CentreFun -> [Char] -> Bool
prop_centre6 wrap xs = length (lines xs) == 
	length (filter (\x -> (x==1)||(x==0)||(x==(-1))) (zipWith (-) blanksHead blanksTail))
	where
	blanksHead = [count x| x <- wrap']
	blanksTail = [count (reverse x)| x <- wrap'] where
	wrap' = lines (wrap xs);
	count [] = 0
	count (x:xs) = if (isSpace x) && x == ' ' then 1 + count xs else 0

prop_centre7 :: CentreFun -> [Char] -> Bool
prop_centre7 wrap xs = True

prop_centre8 :: CentreFun -> [Char] -> Bool
prop_centre8 wrap xs = True

prop_centre9 :: CentreFun -> [Char] -> Bool
prop_centre9 wrap xs = True

prop_centre10 :: wrap xsFun -> [Char] -> Bool
prop_centre10 wrap xs = True

{- H3 -}

rotateCCW :: Picture -> Picture
rotateCCW [] = []
rotateCCW xs = [[last x| x <- xs', length x > 0]] ++ 
	rotateCCW [init x| x <- xs', length x > 1] where 
	xs' = [substitute (x) ++ (replicate (len - (length x))) '.'| x <- xs]
	substitute [] = []
	substitute (x:xs) = if isSpace x then '.' : substitute xs else x : substitute xs
	len = foldr1 (max) [length x| x <- xs]

{- H4 -}

{-WETT-}
encode :: String -> String -> String
encode key cleartext = [key !! (ord (x) - 97)| x <- cleartext]

decode :: String -> String -> String
decode key cryptotext = [chr $ (head (elemIndices x key)) + 97| x <- cryptotext]
{-TTEW-}

-- QuickCheck test für Codierungsprogramm
prop_coding :: String -> String -> Property
prop_coding xs ys = ([x | x <- ys, (ord x) - 97 < length xs, (ord x) - 97 >= 0] == ys) 
	&& (null xs) &&  (null ys)
	==> decode xs (encode xs ys) == ys
