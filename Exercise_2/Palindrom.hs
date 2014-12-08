module Palindrom where
import Data.List
import Test.QuickCheck
import Data.Function 
import Exercise_2

palindromesOfRadius' :: [Char] -> Integer -> [[Char]]
palindromesOfRadius' alphabet 0 = [""] ++ [[a]| a <- alphabet]
palindromesOfRadius' alphabet radius = sortBy (compare `on` length) $ nub $ (palindromesOfRadius' alphabet (radius - 1)) 
	++ (zipWith (++) (perGen head ([[x]| x <- alphabet] ++ [""])) tail)
	where head = wordsOfLength alphabet radius; 
		tail = concat [replicate (1 + length alphabet) (reverse x)| x <- head]

palindromesOfRadius'' :: [Char] -> [[Char]] -> Integer -> [[Char]]
palindromesOfRadius'' alphabet head 0 = [""] ++ [[a]| a <- alphabet]
palindromesOfRadius'' alphabet head radius =  (palindromesOfRadius'' alphabet (nub [init x| x <- head]) (radius - 1)) 
	++ (zipWith (++) (perGen head ([[x]| x <- alphabet] ++ [""])) tail) 
		where tail = concat [replicate (1 + length alphabet) (reverse x)| x <- head]
		
palindromesOfRadius''' :: [Char] -> Integer -> [[Char]]
palindromesOfRadius''' alphabet radius =
	(if radius == 0 then [] else palindromesOfRadius''' alphabet $ radius - 1) ++
	[w ++ mid ++ reverse w | mid <- "" : map (: "") (nub alphabet),
	w <- wordsOfLength alphabet radius]