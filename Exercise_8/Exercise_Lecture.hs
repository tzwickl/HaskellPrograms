module Exercise_Lecture where

data Tree a = 
	Empty | 
	Node a (Tree a) (Tree a)
	deriving (Eq, Show)
	
-- checks if a Node does exist in a tree
find :: Ord a => a -> Tree a -> Bool
find _ Empty = False
find y (Node x l r) 
	| y < x = find y l
	| y > x = find y r
	| otherwise = True

-- inserts a new Node to a tree
insert :: Ord a => a -> Tree a -> Tree a
insert y Empty = Node y Empty Empty
insert y (Node x l r) 
	| y < x = Node x (insert y l) r
	| y > x = Node x l (insert y r)
	| otherwise = Node x l r
	
-- deletes a Node from a tree
delete :: Ord a => a -> Tree a -> Tree a
delete _ Empty = Empty
delete y (Node x l r) 
	| x == y = combine l r
	| y < x = Node x (delete y l) r
	| otherwise = Node x l (delete y r)
	
combine :: Ord a => Tree a -> Tree a -> Tree a
combine Empty t = t
combine t Empty = t
combine l r = Node m l' r where
	(m, l') = leftL l
	
leftL :: Ord a => Tree a -> (a, Tree a)
leftL (Node x l Empty) = (x, l)
leftL (Node x l r) = (m, Node x l r') where
	(m, r') = leftL r