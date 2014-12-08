module Form where
import Data.List (nub)
import Control.Monad
import Test.QuickCheck

type Name = String
data Form = F | T | Var Name | Not Form | Form :&: Form | Form :|: Form
          deriving Eq

instance Show Form where
  show F = "F"
  show T = "T"
  show (Var x) = x
  show (Not p) = par("~" ++ show p)
  show (p :&: q) = par(show p ++ " & " ++ show q)
  show (p :|: q) = par(show p ++ " | " ++ show q)

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

vars :: Form -> [Name]
vars F = []
vars T = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (p :&: q) = nub (vars p ++ vars q)
vars (p :|: q) = nub (vars p ++ vars q)

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
  isOp (Not p)    =  True
  isOp (p :&: q)  =  True
  isOp (p :|: q)  =  True
  isOp p          =  False
isSimple (p :&: q)  =  isSimple p && isSimple q
isSimple (p :|: q)  =  isSimple p && isSimple q
isSimple p          =  True

simplify :: Form -> Form
simplify (Not p)    =  pushNot (simplify p)
  where
  pushNot (Not p)    =  p
  pushNot (p :&: q)  =  pushNot p :|: pushNot q
  pushNot (p :|: q)  =  pushNot p :&: pushNot q
  pushNot p          =  Not p
simplify (p :&: q)  =  simplify p :&: simplify q
simplify (p :|: q)  =  simplify p :|: simplify q
simplify p          =  p

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
             liftM2 (:|:) (prop(n `div` 2)) (prop(n `div` 2))]

prop_simplify p = isSimple(simplify p)
