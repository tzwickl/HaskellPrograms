module TExercise_8 where

{- Library -- nicht veraendern -}

data Tree a =
  Empty |
  Node a (Tree a) (Tree a)
    deriving (Eq, Show)

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node a l r)
  | x < a = Node a (insert x l) r
  | x > a = Node a l (insert x r)
  | otherwise = Node a l r

{- End Library -}



-- WICHTIG
-- Dieses Template ist nur fuer die Gruppenaufgaben gedacht!

{- G2 -}

isOrderedTree :: Ord a => Tree a -> Bool
isOrderedTree t = undefined

treeSort :: Ord a => [a] -> [a]
treeSort xs = undefined