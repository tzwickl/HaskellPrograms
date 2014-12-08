quasiMajorityElems :: Eq a => [a] -> [a]
quasiMajorityElems xs = aux (H.lookup max hm)  where
	max = if null keys then 0 else maximum keys
	keys = (H.keys hm)
	hm = (getMajorityElem H.empty H.empty xs)
	aux (Just x) = x
	aux (Nothing) = []

getMajorityElem :: Eq a => H.HashMap a Int -> H.HashMap Int [a] -> [a] -> H.HashMap Int [a]
getMajorityElem _ hm2 [] = hm2
getMajorityElem hm1 hm2 (x:xs) = getMajorityElem hm1' hm2' xs where
	(hm1', hm2') = addToHashMap x hm1 hm2

addToHashMap :: Eq a => a -> H.HashMap a Int -> H.HashMap Int [a] -> (H.HashMap a Int, H.HashMap Int [a])
addToHashMap x hm1 hm2 = (hm1', hm2') where 
	hm1' = H.insertWith (\x y -> x + y) x 1 hm1
	frequency = aux (H.lookup x hm1') where
		aux (Just x)  = x
		aux (Nothing) = 0
	hm2' = H.insertWith (\xs ys -> (xs ++ ys)) frequency [x] hm2