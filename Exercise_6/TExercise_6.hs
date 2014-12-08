module HExercise_6 where

{- Aufgabe 6.4 -}
partition :: (a -> Bool) -> [a] -> ([a],[a])
partition f as = (foldr (\x -> if f x then (x:) else id) [] as, 
	foldr (\x -> if not (f x) then (x:) else id) [] as)
	
partition' :: (a -> Bool) -> [a] -> ([a],[a])
partition' f as = foldr (\x (fs, ss) -> if f x then (x:fs, ss) else (fs, x:ss)) ([],[]) as