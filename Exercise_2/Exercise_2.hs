module Exercise_2 where
import Data.List
import Test.QuickCheck
import Data.Function 


{- H1 -}

count :: [Char] -> Char -> Integer
count cs c = genericLength [x| x <- cs, c == x]

{- H2 -}

reverseLookup :: Integer -> [(String, Integer)] -> [String]
reverseLookup size tab = [fst t| t <- tab, snd t == size]

-- Teilaufgabe a
-- pr¸ft ob alle B¸rger mit der gesuchten Größe durch die Funktion gefunden wurden
prop1 :: Integer -> [(String, Integer)] -> Property
prop1 s xs = (s > 0) && not(null xs) ==> 
	reverseLookup s xs == [fst x| x <- xs, snd x == s]

-- Teilaufgabe b
-- prüft ob die gefundenen Bürger auch wirklich die gesuchte Schuhgröße besitzen
prop2 :: Integer -> [(String, Integer)] -> Property
prop2 s xs = (s > 0) && not(null xs) ==> 
	[x1| x1 <- y, x2 <- xs, x1 == fst x2, snd x2 == s] == reverseLookup s xs
	where y = reverseLookup s xs
	
{- Durch den zus‰tzlichen Parameter maxSize=4 begrenzt man den Bereich aus dem die Zufallszahlen ausgewählt werden und die Größe der Liste.
   Dadurch dass man den Bereich einschränkt aus dem Zufallszahlen ausgewählt werden können erhöht sich auch die Wahrscheinlichkeit, dass
   Zahlen ausgewählt werden die auch wirklich zu einem Ergebnis in der Funktion reverseLookup führen. Da ja die Schuhgröße in der Liste 
   mit der im ersten Parameter übereinstimmen muss. Ohne der Begrenzung würden so riesige Zahlen ausgewählt werden dass es ziemlich
   unwahrscheinlich ist einmal zwei zufällig selbe Zahlen zu bekommen und somit die Tests nicht sehr effizient sind.
-}

{- H3 -}
-- foldr1 wendet die Funktion perGen zuerst auf die letzten zwei Listenelemente an, danach übergibt es der Funktion das Ergebnis
-- und das drittletzte Listenelement. Das geht so weiter bis zum Anfang der Liste. 
-- Replicate macht nichts anderes als die ¸bergebene Liste n-Mal zu kopieren
wordsOfLength :: [Char] -> Integer -> [[Char]]
wordsOfLength xs 0 = [""]
wordsOfLength xs n 
	| n < 0 = undefined
	| otherwise = nub $ foldr1 (perGen) $ replicate (fromIntegral n) x
	where x = [[x]| x <- xs]

-- Hilfsfunktion die alle mˆglichen Kombination der Form ["a1b1", "a1b2", "a2b1", "a2b2"] liefert
perGen :: [[Char]] -> [[Char]] -> [[Char]]
perGen xs ys = [a ++ b| a <- xs, b <- ys]

wordsOfLength' :: [Char] -> Integer -> [[Char]]
wordsOfLength' alphabet 0 = [[]]
wordsOfLength' alphabet n = [[a] ++ s | a <- alphabet, s <- wordsOfLength' alphabet (n - 1)]
{- H4 -}

{-WETT-}
-- Zuerst werden alle Palindrome erstellt mit dem maximalen Radius, plus einem Element in der Mitte. Die Funktion wordsOfLength generiert
-- dazu erstmal alle möglichen Kombinationen (Permutation des alphabet mit der Länge Radius). Das Ergebnis wird dann mithilfe der
-- perGen Funktion mit dem alphabet und der leeren Zeichenkette konkateniert. Anschlieﬂend wird die umgedrehte Zeichenkette
-- des vorderen Palindromes angehängt. 
-- Dieser Vorgang wird dann redundant solange wiederholt bis der Radius gleich 0 erreicht wird
-- Zum Schluss werden dann alle doppelten Palindrome gelöscht und die Liste der Länge nach aufsteigend sortiert.
palindromesOfRadius :: [Char] -> Integer -> [[Char]]
palindromesOfRadius alphabet 0 = [""] ++ [[a]| a <- alphabet]
palindromesOfRadius alphabet radius = nub $ (palindromesOfRadius alphabet (radius - 1)) 
	++ (sortBy (compare `on` length) (zipWith (++) (perGen head ([[x]| x <- alphabet] ++ [""])) tail))
	where head = wordsOfLength alphabet radius; 
		tail = concat [replicate (1 + length alphabet) (reverse x)| x <- head]
{-TTEW-}