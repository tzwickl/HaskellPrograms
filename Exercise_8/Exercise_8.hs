module Exercise_8 where
import Control.Monad
import Data.Ratio
import Test.QuickCheck
import Data.Maybe
import qualified Data.Sequence as S

{- Library -- nicht veraendern -}

data PosShape = At Shape (Integer, Integer)

data Shape =
  Circle Integer |
  Rectangle Integer Integer -- Breite, Höhe
  deriving (Show, Eq)

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

delete :: Ord a => a -> Tree a -> Tree a
delete x Empty = Empty
delete x (Node a l r)
  | x == a     =  combine l r
  | x < a      =  Node a (delete x l) r
  | otherwise  =  Node a l (delete x r)

combine :: Tree a -> Tree a -> Tree a
combine Empty r  =  r
combine l Empty  =  l
combine l r      =  Node m l r'  where (m,r') = delL r

delL :: Tree a -> (a, Tree a)
delL (Node a Empty r)  = (a, r)
delL (Node a l     r)  = (m, Node a l' r)  where (m,l') = delL l

-- allow QuickCheck to generate arbitrary values of type Tree
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized tree
    where
    tree 0 = return Empty
    tree n | n > 0 = oneof [return Empty,
      liftM3 Node arbitrary (tree (n `div` 2)) (tree (n `div` 2))]

data RegEx =
  Any |
  One Char |
  OneIn [(Char, Char)] |
  Concat RegEx RegEx |
  Alt RegEx RegEx |
  Repeat RegEx

{- End Library -}



-- WICHTIG
-- Dieses Template ist nur fuer die Hausaufgaben gedacht!

{- H1 -}
-- checks if a tree satisfies the invariant of being a Heap
-- a min heap is defined as following: all children of a node are smaller than the node itself
isHeap :: Ord a => Tree a -> Bool
isHeap t = go Nothing Nothing t where
	go :: Ord a => Maybe a -> Maybe a -> Tree a -> Bool
	go _ _ Empty        = True
	go x z (Node y l r) =
		x << (Just y) &&
		z << (Just y) &&
		go x (Just y) l &&
		go (Just y) z r

	(<<) :: Ord a => Maybe a -> Maybe a -> Bool
	Just a << Just b = a < b
	_      << _      = True


{- H2 -}

data Direction = L | R
  deriving (Show, Eq)

-- replaces a tree on the position described by the direction instructions
-- if successful the new tree is returned else Nothing is returned
replace :: [Direction] -> Tree a -> Tree a -> Maybe (Tree a)
replace [] t' t = Just (t')
replace _ t' Empty = Nothing
replace (L:ds) t' (Node x l r) = if isJust(l') then Just (Node x (fromJust (l')) r) else Nothing where
	l' = replace ds t' l
replace (R:ds) t' (Node x l r) = if isJust(r') then Just (Node x l (fromJust (r'))) else Nothing where 
	r' = replace ds t' r


{- H3 -}
-- is the same as the usual find function but this time implemented with the data type compare
find' :: Ord a => a -> Tree a -> Bool
find' _ Empty = False
find' y (Node x l r) = go (compare y x) where
	go LT = find' y l
	go GT = find' y r
	go EQ = True

{- H4 -}
-- creates a maybeMap where the function f is applied to each element of the array
-- if one number results in Nothing, Nothing is returned else the maybeMap is returned
maybeMap :: (a -> Maybe b) -> [a] -> Maybe [b]
maybeMap f [] = Just []
maybeMap f (a:as) = if isJust m then if isJust ms then Just ((fromJust m) : (fromJust ms)) 
	else Nothing else Nothing where 
	m = f a
	ms = maybeMap f as

-- computes the inverse of each number in as
-- if one number in the array as is zero Nothing is returned
inverses :: [Integer] -> Maybe [Rational]
inverses as = maybeMap (\x -> if x == 0 then Nothing else Just (1 % x)) as

{- H5 -}
-- user interface of the match function which converts the input String into a Sequence
match :: RegEx -> String -> Bool
match regex ss = snd (match' regex (S.fromList ss))

-- main match function which takes a RegEx, a Sequence and returns a tupel with the remaining Sequence and the evaluation of
-- the last operator
match' :: RegEx -> S.Seq Char -> (S.Seq Char, Bool)
match' Any sq = if S.null sq then (S.empty, False) else (sq', S.null (sq')) where
	sq' = S.drop 1 sq
match' (One x) sq = if S.null sq then (S.empty, False) else (sq', x == y && S.null (sq')) where
	y = S.index sq 0
 	sq' = S.drop 1 sq
match' (OneIn x) sq = if S.null sq then (S.empty, False) else 
 	(sq', (or [(a <= y) && (y <= b)| (a,b) <- x]) && S.null (sq')) where
	y = S.index sq 0
 	sq' = S.drop 1 sq
match' (Concat a b) sq = if S.null sq then (S.empty, False) else 
 	(sq'', (snd (a') && snd(b')) && S.null (sq')) where
 	sq' = S.drop 2 sq
 	sq'' = S.drop 1 sq
 	a' = match' a (S.take 1 sq)
 	b' = match' b (S.take 1 sq'')
match' (Alt a b) sq = if S.null sq then (S.empty, False) else 
 	(sq', (snd (a') || snd(b')) && S.null (sq')) where
 	sq' = S.drop 1 sq
 	a' = match' a sq
 	b' = match' b sq
match' (Repeat a) sq = if S.null sq then (S.empty, False) else 
 	(fst (sq'), snd (sq') && r) where
 	r = snd r'
 	r' = if S.null (fst sq') then (S.empty, True) else (match' (Repeat a) (fst sq'))
 	sq' = go 0 sq
 	-- go calls itself recursive as long as it finds a sequence of characters that is evaluating to True or n exceeds the length
 	-- of the current sequence of characters and returns either the remaining sequence of characters or an empty list
 	go n sq = if (S.null sq) || n > (S.length sq) then (S.empty, False) else 
 		if snd (temp) then (sq',True) else go (n+1) sq where 
 		temp = match' a (S.take n sq)
 		sq' = S.drop n sq
{-TTEW-}
