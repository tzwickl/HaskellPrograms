import Test.QuickCheck hiding (NonEmptyList)

data Tree a = Empty | Node a (Tree a) (Tree a)
	deriving (Eq, Show)

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty      = Node x Empty Empty
insert x (Node a l r)
	| x < a     = Node a (insert x l) r
	| x > a     = Node a l (insert x r)
	| otherwise = Node a l r

{------------------------------------ G1 --------------------------------------}

data Direction = L | R
	deriving (Eq, Show)

navigate :: [Direction] -> Tree a -> Maybe (Tree a)
navigate []       t            = Just t
navigate (d : ds) Empty        = Nothing
navigate (L : ds) (Node x l r) = navigate ds l
navigate (R : ds) (Node x l r) = navigate ds r

{- Optional, ist auch nicht ganz trivial ;-) -}
data Position a = RootNode (Tree a) | InnerNode (Position a) (Tree a)
	deriving (Eq, Show)

getTree :: Position a -> Tree a
getTree (RootNode t)    = t
getTree (InnerNode _ t) = t

goLeft :: Position a -> Maybe (Position a)
goLeft position = case navigate [L] (getTree position) of
	Nothing   -> Nothing
	Just tree -> Just (InnerNode position tree)

goRight :: Position a -> Maybe (Position a)
goRight position = case navigate [R] (getTree position) of
	Nothing -> Nothing
	Just t' -> Just (InnerNode position t')

goParent :: Position a -> Maybe (Position a)
goParent (RootNode _)         = Nothing
goParent (InnerNode parent _) = Just parent

{------------------------------------ G2 --------------------------------------}

isOrderedTree :: Ord a => Tree a -> Bool
isOrderedTree = go Nothing Nothing where
	go :: Ord a => Maybe a -> Maybe a -> Tree a -> Bool
	go _ _ Empty        = True
	go x z (Node y l r) =
		x <<< (Just y) &&
		(Just y) <<< z &&
		go x (Just y) l &&
		go (Just y) z r

	(<<<) :: Ord a => Maybe a -> Maybe a -> Bool
	Just a <<< Just b = a < b
	_      <<< _      = True

{- Kuerzere Loesung bei der Listen als Ersatz fuer Maybe verwendet werden -}
isOrderedTree' :: Ord a => Tree a -> Bool
isOrderedTree' = go [] [] where
	go _ _ Empty        = True
	go x z (Node y l r) =
		all (< y) x &&
		all (> y) z &&
		go x [y] l &&
		go [y] z r

ascending :: Ord a => [a] -> Bool
ascending (x1 : x2 : xs) = x1 < x2 && ascending (x2 : xs)
ascending _              = True

flat :: Tree a -> [a]
flat Empty        = []
flat (Node a l r) = flat l ++ [a] ++ flat r

isOrderedTree'' :: Ord a => Tree a -> Bool
isOrderedTree'' = ascending . flat

treeSort :: Ord a => [a] -> [a]
treeSort xs = flat $ foldl (flip insert) Empty xs

propAscending :: [Integer] -> Bool
propAscending = ascending . treeSort

propTotal :: [Integer] -> Bool
propTotal xs = all (`elem` treeSort xs) xs

{------------------------------------ G3 --------------------------------------}

data NonEmptyList a = a `Cons` [a]
	deriving (Eq, Show)

fromList :: [a] -> Maybe (NonEmptyList a)
fromList []       = Nothing
fromList (x : xs) = Just (x `Cons` xs)

toList :: NonEmptyList a -> [a]
toList (x `Cons` xs) = x : xs

nHead :: NonEmptyList a -> a
nHead (x `Cons` xs) = x

nTail :: NonEmptyList a -> [a]
nTail (x `Cons` xs) = xs

nAppend :: NonEmptyList a -> NonEmptyList a -> NonEmptyList a
nAppend (x `Cons` xs) (y `Cons` ys) = x `Cons` (xs ++ [y] ++ ys)
