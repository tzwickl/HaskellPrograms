-------------------------------------------------------------------------
-- 								
--         Huffman coding in Haskell.					
-- 								
--         Based on the files by Simon Thompson
-- 							
-------------------------------------------------------------------------

module Huffman (Bit(..), Tree(..), encode, decode, mkTree, mkCTable, mkFTable, serializeTree, deserializeTree)
where

import Control.Applicative ((<$>),(<*>))
import Data.Binary (Binary, Get, put, get)
import qualified Data.Binary as BI
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word8)
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Test.QuickCheck

-- Interface

encode :: CTable -> String -> [Bit]
decode :: Tree -> [Bit] -> String
mkTree :: FTable -> Tree
mkCTable :: Tree -> CTable
mkFTable :: String -> FTable
serializeTree :: Tree -> BS.ByteString
deserializeTree :: BS.ByteString -> Tree

data Tree = Leaf Char Int | Node Tree Tree Int

data Bit = L | R  deriving (Eq,Show)

instance Arbitrary Bit where
  arbitrary = elements [L, R]

type CTable = [ (Char,[Bit]) ]
type FTable = [ (Char, Int) ]


instance Binary Tree where
  put (Leaf c i) = do
    put (0 :: Word8)
    put c
    put i
  put (Node t1 t2 i) = do
    put (1 :: Word8)
    put t1
    put t2
    put i

  get = do
    tag <- get :: Get Word8
    case tag of
      0 -> Leaf <$> get <*> get
      1 -> Node <$> get <*> get <*> get


-- Implementation

-- Encoding a string

encode tbl = concat . map (\c -> fromJust (lookup c tbl))

-- Decoding a string

decode tr = dec tr
  where
  dec (Node t1 t2 _) (L:rest) = dec t1 rest
  dec (Node t1 t2 _) (R:rest) = dec t2 rest
  dec (Leaf c _) rest = c : decode tr rest
  dec _ [] = []

-- Huffman's algorithm: Building the code tree
-- Convert the trees to a list, then combine into a single tree.

mkTree = combine . toTrees

-- Huffman codes are created bottom up: look for the least	
-- two frequent letters, make these a new Node
-- and repeat until one tree is formed.	

-- The function toTreeList makes the initial data structure.

toTrees :: FTable -> [Tree]

toTrees = map (uncurry Leaf) . sortBy (\(_,m) (_,n) -> compare m n)

-- The value of a tree.

value :: Tree -> Int

value (Leaf _ n)   = n
value (Node _ _ n) = n

-- Pair two trees.

pair :: Tree -> Tree -> Tree

pair t1 t2 = Node t1 t2 (v1+v2)
             where
             v1 = value t1
             v2 = value t2

-- Insert a tree in a list of trees sorted by ascending value.

insTree :: Tree -> [Tree] -> [Tree]

insTree t [] = [t]
insTree t (t1:ts) 
  | (value t <= value t1)    = t : t1 : ts
  | otherwise                = t1 : insTree t ts
-- 	
-- Combine the front two elements of the list of trees.

combine2 :: [Tree] -> [Tree]

combine2 (t1 : t2 : ts) = insTree (pair t1 t2) ts

-- Combine the whole list.				

combine :: [Tree] -> Tree

combine [t] = t
combine ts = combine (combine2 ts)


-- Making a code table from a Huffman tree.

mkCTable = mkCTab []

-- Auxiliary function used in conversion to a code table. The first argument
-- is the Bit list which codes the path in the tree to the current Node,
-- and so mkCTable initialises it with the empty list.

mkCTab :: [Bit] -> Tree -> CTable

mkCTab cd (Leaf c _) =  [(c,cd)]
mkCTab cd (Node t1 t2 _)
	= (mkCTab (cd++[L]) t1) ++ (mkCTab (cd++[R]) t2)


-- Show a tree, using indentation to show structure.

instance Show Tree where
  show t = showIndent 0 t
-- The auxiliary function showIndent has a second, current
-- level of indentation, as a parameter.
    where
    showIndent :: Int -> Tree -> String
    showIndent m (Leaf c n) = spaces m ++ show c ++ " " ++ show n ++ "\n"
    showIndent m (Node t1 t2 n)
      = showIndent (m+4) t1 ++
        spaces m ++ "[" ++ show n ++ "]" ++ "\n" ++
        showIndent (m+4) t2
    spaces n = replicate n ' '

-- Build a frequency table table from a string

mkFTable cs = freq cs []

freq :: String -> FTable -> FTable
freq [] tbl = tbl
freq (c:cs) tbl = freq cs (incr c tbl)

incr :: Char -> FTable -> FTable
incr c [] = [(c,1)]
incr c ((d,n):ps) = if c==d then (d,n+1):ps else (d,n) : incr c ps

-- (De)serializing trees

serializeTree = BS.concat . LBS.toChunks . BI.encode
deserializeTree = BI.decode . LBS.fromChunks . (:[])
