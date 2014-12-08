import Data.List
import Data.Ratio

import ExtForm

form = ((Var "A") :&: (Var "B")) :|: ((Not (Var "A")) :&: (Not (Var "B")))

{--------------------------------- G1 -----------------------------------------}

-- siehe ExtForm.hs

{--------------------------------- G2 -----------------------------------------}

data Arith = Arith :+: Arith
	| Arith :*: Arith
	| Constant Integer
	| Variable String

evalA :: [(String, Integer)] -> Arith -> Integer
evalA variables (x :+: y)    = evalA variables x + evalA variables y
evalA variables (x :*: y)    = evalA variables x * evalA variables y
evalA _         (Constant x) = x
evalA variables (Variable k) = case lookup k variables of
	Just v  -> v
	Nothing -> 0

{--------------------------------- G3 -----------------------------------------}

mkTable :: Form -> [[String]]
mkTable phi = firstRow : secondRow : map (zipWith align lengths . mkRow)
	(vals $ vars phi)
		where
			firstRow = vars phi ++ ["|", show phi]
			secondRow = map (map (const '-')) firstRow
			lengths = map length firstRow

			mkRow val = map (stringOfBool . snd) val ++
				["|", stringOfBool $ eval val phi]

			stringOfBool True = "T"
			stringOfBool False = "F"

			align n xs = lpad ++ xs ++ rpad where
				(lpad, rpad) = splitAt ((n - length xs) `div` 2)
					(replicate (n - length xs) ' ')

showTable :: Form -> String
showTable = unlines . map unwords . mkTable

printTable :: Form -> IO ()
printTable = putStr . showTable
