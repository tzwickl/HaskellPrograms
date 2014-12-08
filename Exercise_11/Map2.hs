module Map2 (
	Map,
	empty,
	insert,
	Map2.lookup,
	delete,
	keys,
	mapV,
	-- this function facilitates the conversion from one map to another map
	toList
) where

import Prelude hiding (lookup)

------------------- definition of the data types used in this module ---------------------

-- data type Map
newtype Map k v = Map (Tree k v)
-- data type Tree which represents the internal data-structure of Map
data Tree k v = Empty | Node k v (Tree k v) (Tree k v)
	deriving (Eq, Ord)

-- own show function which prints the Map on the console as a List of tuples (key, value)
-- for a better overview
instance (Show k, Show v, Ord k) => Show (Map k v) where
	show x = "Map " ++ show (toList x)

-------------------------- interface for using this module -------------------------------

empty :: Ord k => Map k v
insert :: Ord k => k -> v -> Map k v -> Map k v
lookup :: Ord k => k -> Map k v -> Maybe v
delete :: Ord k => k -> Map k v -> Map k v
keys :: Ord k => Map k v -> [ k ]
mapV :: Ord k => ( v -> w ) -> Map k v -> Map k w
toList :: Ord k => Map k v -> [(k,v)]

-------------------------- implementation of the interface -------------------------------

-- returns an empty Map
empty = Map Empty

-- inserts a new element into the Map and returns the new Map
-- if the key is already in the Map the value is replaced by the new one
insert k v (Map Empty) = Map (Node k v Empty Empty)
insert k v (Map (Node k' v' left right)) 
	| k == k' 	= (Map (Node k v left right))
	| k <  k' 	= (Map (Node k' v' left' right))
	| otherwise = (Map (Node k' v' left right')) where 
		Map left'  = insert k v (Map left)
		Map right' = insert k v (Map right)
		
-- returns the value on the position defined by the passed key
lookup k (Map Empty) = Nothing
lookup k (Map (Node k' v' left right)) 
	| k == k' = Just v'
	| k < k'  = lookup k (Map left)
	| k > k'  = lookup k (Map right)

-- deletes the element from the Map on the position defined by the passed key
-- if the key isn't available the map is returned unchanged
delete k (Map Empty) = Map Empty
delete k (Map (Node k' v' left right)) 
	| k == k'     = Map $ combine left right
	| k <  k'     = (Map (Node k' v' left' right)) 
	| otherwise   = (Map (Node k' v' left right')) where
		Map left'  = delete k (Map left)
		Map right' = delete k (Map right)

-- returns a list representation of all keys in the Map
keys (Map Empty) = []
keys (Map (Node k _ left right)) = [k] ++ keys (Map left) ++ keys (Map right)

-- applies the passed function to each value and returns the new Map
mapV f (Map Empty) = Map Empty
mapV f (Map (Node k v left right)) = (Map (Node k v' left' right')) where
		v' = f v
		Map left'  = mapV f (Map left)
		Map right' = mapV f (Map right)

-- converts a map to a List of tuples
toList (Map Empty) = []
toList (Map (Node k v left right)) = [(k,v)] ++ toList (Map left) ++ toList (Map right)

---------- auxiliary functions (not accessible from outside of this module) --------------

-- combines two trees to one, there are three cases to considerer
-- first case: the left tree is empty -> the right tree is returned
-- second case: the right tree is empty -> the left tree is returned
-- third case: none of the trees are empty -> we have to find the max Node in the left tree
-- and use this as the root tree to combine the two trees to obtain the order in the tree
combine :: Ord k => Tree k v -> Tree k v -> Tree k v
combine Empty r = r
combine l Empty = l
combine l r 	= (Node k' v' left right) where
	(k', v', left) = findMaxNode l
	right = r
	
-- traverses through the tree and finds the max node in the passed tree 
-- (it's the most-right node in the tree)
-- returns the node's values and the new tree without this node
findMaxNode :: Ord k => Tree k v -> (k, v, Tree k v)
findMaxNode (Node k v left Empty) = (k, v, left)
findMaxNode (Node k v left right) = (k', v', Node k v left right') where
		(k', v', right') = findMaxNode right