import Data.List
import Test.QuickCheck

isAscending :: [Integer] -> Bool
isAscending (x : y : zs) = x <= y && isAscending (y : zs)
isAscending _            = True

hasFibonacciProperty :: [Integer] -> Bool
hasFibonacciProperty (x : y : z : zs) = x + y == z && hasFibonacciProperty (y : z : zs)
hasFibonacciProperty _                = True

{-
Note: [x] is just a syntactic abbreviation for (x : [])

Lemma reverse (snoc xs x) = x : reverse xs
Proof by structural induction on xs

Base case:
To show: reverse (snoc [] x) = x : reverse []
reverse (snoc [] x)
== reverse [x]                 (by snoc_Nil)
== reverse [] ++ [x]           (by reverse_Cons)
== [] ++ [x]                   (by reverse_Nil)
== [x]                         (by append_Nil)
x : reverse []
== [x]                         (by reverse_Nil)

Induction step:
IH: reverse (snoc ys x) = x : reverse ys
To show: reverse (snoc (y : ys) x) = x : reverse (y : ys)

reverse (snoc (y : ys) x)
== reverse (y : snoc ys x)     (by snoc_Cons)
== reverse (snoc ys x) ++ [y]  (by reverse_Cons)
== (x : reverse ys) ++ [y]     (by IH)
== x : (reverse ys ++ [y])     (by append_Cons)

x : reverse (y : ys)
== x : (reverse ys ++ [y])     (by reverse_Cons)
-}

perms :: String -> [String]
perms "" = [""]
perms xs = reverse (sort (nub ([y : ys | y <- xs, ys <- perms (delete y xs)])))

match :: [Char] -> [Char] -> Bool
match [] ys                 = null ys
match ('?' : ps) (_ : ys)   = match ps ys
match ('*' : ps) []         = match ps []
match ('*' : ps) (y : ys)   = match ps (y : ys) || match ('*' : ps) ys
match (p : ps)   []         = False
match (p : ps)   (y : ys)   = p == y && match ps ys
