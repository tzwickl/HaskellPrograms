module AssocList (
	Map,
	empty,
	insert,
	AssocList.lookup,
	delete,
	keys,
	-- this function facilitates the conversion from one map to another map
	toList
) where

import Prelude hiding (lookup)

------------------- definition of the data types used in this module ---------------------

-- data type for association map
newtype Map k v = Map [(k, v)]
	deriving (Eq, Show)

-------------------------- interface for using this module -------------------------------

empty :: Eq k => Map k v
insert :: Eq k => k -> v -> Map k v -> Map k v
lookup :: Eq k => k -> Map k v -> Maybe v
delete :: Eq k => k -> Map k v -> Map k v
keys :: Eq k => Map k v -> [ k ]
toList :: Eq k => Map k v -> [(k, v)]

-------------------------- implementation of the interface -------------------------------

-- creates a new empty association list
empty = Map []

-- inserts a new key/value association
-- if the key is already in the list the value is replaced by the new one
insert key value xs = Map ((key, value) : ys) where
		Map ys = delete key xs
		
-- conducts a lookup in the association list and returns the value if the wanted
-- key is in the list else Nothing is returned
lookup _ (Map []) = Nothing
lookup key (Map (x:xs)) = if key == k then Just v else lookup key (Map xs) where
	(k,v) = x

-- deletes the pair with the passed key if available else the map is returned unchanged
delete key (Map xs) = Map $ filter (\(x, y) -> x /= key) xs

-- returns a list of all keys in the association list
keys (Map []) = []
keys (Map (x:xs)) = [key] ++ keys (Map xs) where
	(key, _) = x
	
-- converts a map to a List of tuples
toList (Map xs) = xs