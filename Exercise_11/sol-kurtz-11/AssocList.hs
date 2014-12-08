module AssocList (
	Map,
	empty,
	insert,
	AssocList.lookup,
	delete,
	keys
) where

data Map k v = Map [(k, v)]
	deriving (Eq, Show)

empty :: Eq k => Map k v
empty = Map []

insert :: Eq k => k -> v -> Map k v -> Map k v
insert k v xs = Map $ (k, v) : ys where
	Map ys = delete k xs

lookup :: Eq k => k -> Map k v -> Maybe v
lookup k (Map xs) = Prelude.lookup k xs

delete :: Eq k => k -> Map k v -> Map k v
delete k (Map xs) = Map $ filter ((/= k) . fst) xs

keys :: Eq k => Map k v -> [k]
keys (Map xs) = map fst xs
