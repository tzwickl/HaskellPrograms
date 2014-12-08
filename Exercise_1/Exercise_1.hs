module Exercise_1 where
import Test.QuickCheck

{- Aufgabe H1.1 -}

{-WETT-}
import Data.List
f124 :: Integer -> Integer -> Integer -> Integer
f124 x y z = head xs + 2 * xs !! 1 + 4 * last xs
	where xs = sort [x, y, z]
{-TTEW-}



{---------------------------------------------------------------------}
{- Aufgabe H1.2 -}

{- Teil 1 -}
f :: Integer -> Integer
f n 
	| n > 100 = n - 10
	| otherwise = f (f (n + 11))

{- Teil 2 -}
{-
 - Schreiben Sie ihre Lösungen in diesen Kommentar
 - Ignores that Haskell actually is lazy
 
die Funktion ruft sich selbst solange auf und addiert 11 zu n, bis n größer als 100 ist
danach werden 10 abgezogen und die Funktion wieder solange mit 11 addiert bis n größer als 100 ist
unabhängig vom Eingabeparamter (solange < 101) endet die Funktion immer mit dem Ergebnis 91 da 111 der einzige Wert ist wo die Rekursion untebrochen wird
==> Beweis: insgesamt wird die Zahl immer um 1 erhöht (da 11 addiert und 10 subtrahiert werden). Es werden zwei "returns" benötigt um beide Funktionsaufrufe, 
die innere f(n + 11) und die äußere f (Ergebnis von f(n + 11)) zu beenden.
Startpunkt f 0: 0 => 11 => 22 => 33 => 44 => 55 => 66 => 77 => 88 => 99
=> f (f ( 99 + 11 = 110 )) => 110 - 10 => f ( 100 ) => 100 + 11 => f (f ( 111 )) => 111 - 10 => f ( 101 ) => 101 - 10 => return 91 
		==> Rekursion beendet ==> 91 wird zurückgegeben
Er springt nun zurück bis wo der lokale n-Wert 88 ist und übergibt der Funktion den Rückgabewert 91
f 91: 91 + 11 = 102 => 102 - 10 = 92 => 92 + 11 = 103 => 103 - 10 = 93 => 93 + 11 = 104 => 104 - 10 = 94 => 94 + 11 = 105 => 105 - 10 = 95 
		=> 95 + 11 = 106 => 106 - 10 = 96 => 96 + 11 = 107 => 107 - 10 = 97 => 97 + 11 = 108 => 108 - 10 = 98 => 98 + 11 = 109 
		=> 109 - 10 = 99 => 99 + 11 = 110 => 110 - 10 = 100 => 100 + 11 = 111 => "111 - 10 = 101 => 101 - 10 = 91" ==> Zweifaches return => Rekursion beendet
Das gleiche passiert dann auch noch mit den lokalen n Werten 77, 66, 55 , 44, 33, 22, 11 und 0 
Danach beendet sich die Funktion f und gibt den Wert 91 zurück

Das gleiche Prinzip gilt dann auch für die Paramter 1, 2, 3, 42 und -10

Bei 101 und 1001 wird der Funktionswert sofort zurückgegeben da n größer als 100 ist und somit kein Rekursionsaufruf zustande kommt
f  101:  91
f 1001: 991 
 -}

{- Teil 3 -}
f' :: Integer -> Integer
f' n 
	| n > 100 = n - 10
	| otherwise = 91

{-   Quickcheck test -}
prop_f :: Integer -> Bool
prop_f x = f x == f' x


{---------------------------------------------------------------------}
{- Aufgabe H1.3 -}

-- defines a function which calculates the binary logarithm of n
ld :: Integer -> Integer
ld 1 = 0
ld n = 1 + ld (n `div` 2)
	