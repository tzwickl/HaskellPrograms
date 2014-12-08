module TExercise_9 where
import Data.List (nub)
import Control.Monad
import Test.QuickCheck

{---------------------------------------Aufgabe G9.1-------------------------------------}

type Name = String
data Form = F | T | Var Name | Not Form | Form :&: Form | Form :|: Form
			| Form :->: Form | Form :<->: Form
          deriving Eq

instance Show Form where
  show F = "F"
  show T = "T"
  show (Var x) = x
  show (Not p) = par("~" ++ show p)
  show (p :&: q) = par(show p ++ " & " ++ show q)
  show (p :|: q) = par(show p ++ " | " ++ show q)
  show (p :->: q) = par(show p ++ " -> " ++ show q)
  show (p :<->: q) = par(show p ++ " <-> " ++ show q)

par :: String -> String
par s = "(" ++ s ++ ")"

-- Wertebelegung
type Valuation = [(Name,Bool)]

eval :: Valuation -> Form -> Bool
eval e F = False
eval e T = True
eval e (Var x) = the(lookup x e) where the(Just b) = b
eval e (Not p) = not(eval e p)
eval e (p :&: q) = eval e p && eval e q
eval e (p :|: q) = eval e p || eval e q
eval e (p :->: q) = not(eval e p) || eval e q
eval e (p :<->: q) = eval e p == eval e q

vars :: Form -> [Name]
vars F = []
vars T = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (p :&: q) = nub (vars p ++ vars q)
vars (p :|: q) = nub (vars p ++ vars q)
vars (p :->: q) = nub (vars p ++ vars q)
vars (p :<->: q) = nub (vars p ++ vars q)

vals :: [Name] -> [Valuation]
vals []	= [[]]
vals (x:xs) = [ (x,False):e | e <- vals xs ] ++ [ (x,True ):e | e <- vals xs ]

satisfiable :: Form -> Bool
satisfiable p = or [eval e p | e <- vals(vars p)]

tautology :: Form -> Bool
tautology = not . satisfiable . Not

prop_vals1 xs =
  length(vals xs) == 2 ^ length xs

prop_vals2 xs =
  distinct (vals xs)

distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct (x:xs) = not(elem x xs) && distinct xs

prop_vals1' xs =
  length xs <= 10 ==>
  length(vals xs) == 2 ^ length xs

prop_vals2' xs =
  length xs <= 10 ==> distinct (vals xs)

isSimple :: Form -> Bool
isSimple (Not p)    =  not (isOp p)
  where
  isOp (Not p)     = True
  isOp (p :&: q)   = True
  isOp (p :|: q)   = True
  isOp (p :->: q)  = True
  isOp (p :<->: q) = True
  isOp (T)         = True
  isOp (F)         = True
  isOp p           = False
isSimple (p :&: q)   = isSimple p && isSimple q
isSimple (p :|: q)   = isSimple p && isSimple q
isSimple (p :->: q)  = isSimple p && isSimple q
isSimple (p :<->: q) = isSimple p && isSimple q
isSimple p           = True

simplify :: Form -> Form
simplify (Not p)    =  pushNot (simplify p)
  where
  pushNot (Not p)     =  p
  pushNot (p :&: q)   =  pushNot p :|: pushNot q
  pushNot (p :|: q)   =  pushNot p :&: pushNot q
  pushNot (p :->: q)  =  p :&: pushNot q
  pushNot (p :<->: q) =  (pushNot p :&: q) :|: (p :&: pushNot q)
  pushNot (T)		  = F
  pushNot (F) 		  = T
  pushNot p           =  Not p
simplify (p :&: q)   =  simplify p :&: simplify q
simplify (p :|: q)   =  simplify p :|: simplify q
simplify (p :->: q)  =  simplify p :->: simplify q
simplify (p :<->: q) =  simplify p :<->: simplify q
simplify p           =  p

-- allow QuickCheck to generate arbitrary values of type Form
instance Arbitrary Form where
  arbitrary = sized prop
    where
    name = sized $ \size -> elements (map (:[]) $ take (size + 1) ['a'..'z'])
    prop 0  =
      oneof [return F,
             return T,
             liftM Var name]
    prop n | n > 0 =
      oneof [return F,
             return T,
             liftM Var name,
             liftM Not (prop (n-1)),
             liftM2 (:&:) (prop(n `div` 2)) (prop(n `div` 2)),
             liftM2 (:|:) (prop(n `div` 2)) (prop(n `div` 2)),
             liftM2 (:->:) (prop(n `div` 2)) (prop(n `div` 2)),
             liftM2 (:<->:) (prop(n `div` 2)) (prop(n `div` 2))]

prop_simplify p = isSimple(simplify p)

prop_simplify_sound p = [eval e p | e <- vals(vars p)] == [eval e p' | e <- vals(vars p')]
	where p' = simplify p
	
prop_simplify_sound' bs p = not (null bs) ==> eval val p == eval val (simplify p)
  where val = zip (vars p) (cycle bs)

{---------------------------------------Aufgabe G9.2-------------------------------------}

-- defines the new data type for arithmetical expressions
data Arith = Arith :+: Arith | Arith :-: Arith | Arith :*: Arith | Arith :/: Arith 
			 | Constant Double | Variable Name | Arith :^: Integer
			 
-- defines how the arithmetical expression are printed on the console
instance Show Arith where
	show (Variable x) = x
	show (Constant x) = show x
	show (x :+: y) = par(show x ++ " + " ++ show y)
	show (x :*: y) = par(show x ++ " * " ++ show y)
	show (x :-: y) = par(show x ++ " - " ++ show y)
	show (x :/: y) = par(show x ++ " / " ++ show y)
	show (x :^: y) = par(show x ++ " ^ " ++ show y)
			 
-- evaluates an arithmetical expression according to the usual arithmetical rules
-- if a division through 0 is executed Nothing is returned
-- if a negative exponent is passed to the operator Pow Nothing is returned
evalA :: [(Name, Double)] -> Arith -> Maybe Double
evalA e (Variable x) = Just (aux(lookup x e)) where
	aux (Just b) = b
	aux (Nothing) = 0.0
evalA e (Constant x) = Just x
-- plus is an overloaded addition operation for the type Maybe
evalA e (x :+: y) = evalA e x `plus` evalA e y where
	plus :: Fractional a => Maybe a -> Maybe a -> Maybe a
	plus (Just a) (Just b) = Just (a + b)
	plus _ _ = Nothing
	-- mul is an overloaded multiplication operation for the type Maybe
evalA e (x :*: y) = evalA e x `mul` evalA e y where
	mul :: Fractional a => Maybe a -> Maybe a -> Maybe a
	mul (Just a) (Just b) = Just (a * b)
	mul _ _ = Nothing
	-- min is an overloaded minus operation for the type Maybe
evalA e (x :-: y) = evalA e x `min` evalA e y where
	min :: Fractional a => Maybe a -> Maybe a -> Maybe a
	min (Just a) (Just b) = Just (a - b)
	min _ _ = Nothing
	-- div is an overloaded division operation for the type Maybe
evalA e (x :/: y) = evalA e x `div` y' where
	y' = aux (evalA e y)
	aux (Just a) = if a == 0 then Nothing else Just a
	div :: Fractional a => Maybe a -> Maybe a -> Maybe a
	div (Just a) (Just b) = Just (a / b)
	div _ _ = Nothing
	-- pow is an overloaded power operation for the type Maybe
evalA e (x :^: y) = if y < 0 then Nothing else evalA e x `pow` Just (fromIntegral y) where
	pow :: (Fractional a, Eq a) => Maybe a -> Maybe a -> Maybe a
	pow (Just a) (Just b) = Just (power a b) where
			power a 0 = 1
			power a b = a * power a (b-1)
	pow _ _ = Nothing

-- calculates all values of a equation from the start point to the end point with the passed stepRate
-- as a variable only x is allowed in the equation
-- for example if you want to print all values from 0 to 100 with a step-rate of 1 you invoke the function as following:
-- evalFunction (0, 100) 1 Equation
evalFunction :: (Double, Double) -> Double -> Arith -> [(Double, Double)]
evalFunction (start, end) stepRate exp = [(aux' x, aux (evalA x exp))| x <- values start end stepRate] where
	values s e st 
		| s <= e = [[("x", s)]] ++ values (s+st) e st
		| s > e = []
	aux (Just x) = x
	aux (Nothing) = 0.0
	aux' ((_,x):xs) = x
	aux' _ = 0.0

-- creates a formatted String to print on the screen
makeResultTable :: (Double, Double) -> Double -> Arith -> String
makeResultTable (start, end) stepRate exp = firstRow ++ secondRow ++ remainingRows result where
	firstRow = (replicate ((maxWidthFRow `div` 2) - 2) ' ') ++ "x" ++ (replicate (3) ' ') 
		++ "|" ++ show exp ++ "\n"
 	secondRow = (replicate (maxWidthFRow) '-') ++ (replicate (maxWidthSRow) '-') ++ "\n"
	remainingRows [] = ""
	remainingRows ((a,b):rs) = (replicate (currentWidth1) ' ') ++ show a ++  (replicate (2) ' ') ++ "|"
		++ (replicate (maxWidthSRow `div` 2) ' ') ++ show b ++ (replicate (currentWidth2) ' ') 
		++ "\n" ++ remainingRows rs where
			currentWidth1 = (maxWidthFRow `div` 2) - (length $ show a)
			currentWidth2 = (maxWidthSRow `div` 2) - (length $ show b)
	maxWidthFRow = (maximum [length $ show x| (x,_) <- result]) + 6
	maxWidthSRow = maximum ([length $ show x| (_,x) <- result] ++ [length $ show exp])
	result = evalFunction (start, end) stepRate exp

-- prints the result table to the screen
printResult :: (Double, Double) -> Double -> Arith -> IO ()
printResult borders stepRate exp = putStr $ makeResultTable borders stepRate exp



{---------------------------------------Aufgabe G9.3-------------------------------------}
-- returns a formatted String with a truth table and the results of the passed equation
showTable :: Form -> String
showTable p = firstRow ++ secondRow ++ remainingRows truthValues result where
	firstRow = (concat [" " ++ v ++ " " | v <- vars p]) ++ "| " ++ show p ++ "\n"
	secondRow = (concat [" " ++ "-" ++ " " | _ <- [1..length $ vars p]]) ++ "- " ++
		(replicate (lengthEquation) '-') ++ "\n"
	remainingRows _ [] = ""
	remainingRows (e:es) (p:ps) = (concat [" " ++ showT v ++ " " | (_,v) <- e]) ++ "| " ++
		(replicate (lengthEquation `div` 2) ' ') ++ showT p ++ "\n" ++ remainingRows es ps
	lengthEquation = length $ show p
	truthValues = vals(vars p)
	result = [eval e p | e <- vals(vars p)]
	showT (True) = "T"
	showT (False) = "F"

-- prints the truth table formatted on the screen	
printTable :: Form -> IO ()
printTable p = putStr $ showTable p