module Huffman (
	Bit,
	getTree,
	encode,
	decode,
	serializeTree,
	deserializeTree,
	serializeBits,
	deserializeBits
) where

import qualified Data.HashMap.Lazy as HM
import qualified PriorityQueue as PQ
-- modules necessary for de/serialisation
import Control.Applicative ((<$>),(<*>))
import Data.Binary (Binary, Get, put, get)
import qualified Data.Binary as BI
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word8)
import Data.Bits
import Data.List.Split (chunksOf)

----------------------------------- Module Description -----------------------------------
{-
This program calculates a Huffman coding tree by counting the frequency of each letter
in a passed string and creates according to this tree a coding table which is used to
encode the passes string.
Afterwards with the Huffman coding tree and the encoded bit sequence the String can be restored
with the decoding function. If the Huffman coding tree get lost the encoded bit sequence
cannot restored.
In computer science and information theory, Huffman coding is an entropy encoding algorithm 
used for lossless data compression. The term refers to the use of a variable-length code 
table for encoding a source symbol (such as a character in a file) where the variable-length 
code table has been derived in a particular way based on the estimated probability of 
occurrence for each possible value of the source symbol. 
-}
--------------------------------- Data Type definitions ----------------------------------

-- Bit represents the encoded string
data Bit = L | R
	deriving (Eq)
	
-- defines how the Bit values are shown on the console
instance Show Bit where
	show L = "0"
	show R = "1"
	
{- Define a suitable representation for one byte. "Data.Word" might be helpful. -}
type Byte = Word8

-- frequency table represents the frequency of each letter in the to encoding string
type FTable = [(Char, Int)]

-- coding table represents the Bit code for each character calculated by the Huffman algorithm
type CTable = [(Char, [Bit])]

-- Tree represents the Huffman tree generated by the Huffman algorithm
data Tree = Leaf Char Int | Node Int Tree Tree
	deriving Show

-- defines when two Trees are equal
instance Eq Tree where
	(==) (Node i1 _ _) (Node i2 _ _) 	= i1 == i2
	(==) (Leaf _ i1)   (Leaf _ i2) 		= i1 == i2
	(==) (Node i1 _ _) (Leaf _ i2)		= i1 == i2
	(==) (Leaf _ i1)   (Node i2 _ _)	= i1 == i2
	
-- defines how two Trees are compared
instance Ord Tree where
	(<=)  (Leaf _ i1)   (Leaf _ i2)  	= i1 <= i2
	(<=)  (Node i1 _ _) (Node i2 _ _) 	= i1 <= i2
	(<=)  (Node i1 _ _) (Leaf _ i2)		= i1 <= i2
	(<=)  (Leaf _ i1)   (Node i2 _ _)	= i1 <= i2
	
-- defines how the tree is de/serialised
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

-------------------------------------- User Interface ------------------------------------

getTree			:: String -> Tree
encode 			:: Tree -> String -> [Bit]
decode 			:: Tree -> [Bit] -> String
serializeTree 	:: Tree -> BS.ByteString
deserializeTree :: BS.ByteString -> Tree
serializeBits	:: [Bit] -> BS.ByteString
deserializeBits :: BS.ByteString -> [Bit]

-------------------------- Implementation of the Interface -------------------------------

-- crates the frequency table and the Huffman code tree and returns the tree
-- which is required to decode the encoded string
getTree = mkTree . mkFTable

-- this function takes a string and a suitable coding tree and returns the coding of 
-- the passed String
-- with an invalid coding tree there is no guarantee that the result is correct nor
-- if the encoding is successful
encode t str = concat $ map (\c -> fromJust $ lookup c ct) str where
	fromJust (Just x) = x
	ct = mkCTable t

-- this function takes the coding of a string and a suitable coding tree and returns
-- the decoded string
-- with an invalid coding tree there is no guarantee that the result is correct nor
-- if the decoding is successful
decode tr = dec tr where
	dec (Leaf c _) 	 bs 	= [c] ++ dec tr bs
	dec (Node _ l r) (L:bs)	= dec l bs
	dec (Node _ l r) (R:bs) = dec r bs
	dec _			 []		= []
	
-- serialises a passed tree to save it in a file
serializeTree = BS.concat . LBS.toChunks . BI.encode

-- deserialises a ByteString of a tree and returns the tree
deserializeTree = BI.decode . LBS.fromChunks . (:[])

-- serialises a passed bit sequence to a ByteString
serializeBits	= BS.pack . bitsToBytes

-- deserialise a passed ByteString to a sequence of bits
deserializeBits = bytesToBits . BS.unpack

----------- auxiliary functions (not accessible from outside of this module) -------------

-- mkFTable counts the frequency of the different letters in the String and
-- returns a frequency table which consists of each letter and its frequency
-- for efficiency this function does harness the runtime complexity of a HashMap for counting
-- the frequency of each letter in the passed String
mkFTable :: String -> FTable
mkFTable str = HM.toList hashMap where
	hashMap = count str HM.empty
	count :: String -> HM.HashMap Char Int -> HM.HashMap Char Int
	count [] hm 	 = hm
	count (s:str) hm = count str (HM.insertWith (\old new -> old + new) s 1 hm)

-- creates a Huffman tree according to the passed frequency Table by using the Huffman algorithm
mkTree :: FTable -> Tree
mkTree = combine . toTrees

-- toTrees converts each character/frequency pair to a leaf and inserts each leaf into a
-- priority queue
-- the idea behind this method of creating a Huffman tree is that we start to build
-- up the tree from the bottom by combining each leaf with the fewest frequency
-- and work up to the root of the tree
toTrees :: FTable -> PQ.PrioQueue Tree
toTrees ft = createPQ ft PQ.empty where
	createPQ :: FTable -> PQ.PrioQueue Tree -> PQ.PrioQueue Tree
	createPQ [] pq			= pq
	createPQ ((c,i):ft) pq	= createPQ ft (PQ.insert (Leaf c i) pq)

-- combines each Leaf/Node with the lowest frequency as long as there is only one
-- hole tree left and returns this tree
-- this is the main Huffman algorithm which combines each leaf and combines two leafs or
-- nodes with the lowest frequency as long as there are more than one leaf/node left
combine :: PQ.PrioQueue Tree -> Tree
combine pq = go pq where
	go :: PQ.PrioQueue Tree -> Tree
	go pq 
		| PQ.isEmpty newPQ = fst $ PQ.extractMin pq
		| otherwise  = go newPQ where
			newPQ = combine2 pq

-- combines the first two trees consisting in the priority tree (the first two trees in the
-- priority queue are the trees with the smallest frequency)
-- this two trees are combined by adding up their frequency and add them into a new node
-- afterwards the new formed tree is inserted in the priority queue and returned
-- if only one tree is left in the priority queue an empty priority queue is returned to indicate
-- that the process of building up the Huffman code tree has been finished
combine2 :: PQ.PrioQueue Tree -> PQ.PrioQueue Tree
combine2 pq 
	| PQ.isEmpty firstPQ = firstPQ
	| otherwise 		 = PQ.insert (pair t1 t2) secPQ where
		(t1, firstPQ) = PQ.extractMin pq
		(t2, secPQ)   = PQ.extractMin firstPQ

-- combines two trees to one node and adds up the frequency of the two trees to insert it in 
-- the new node
pair :: Tree -> Tree -> Tree
pair t1 t2 = Node ((value t1) + (value t2)) t1 t2

-- returns the frequency of the current node/leaf
value :: Tree -> Int
value (Leaf _ i1)  	= i1
value (Node i1 _ _)	= i1

-- creates the encoding table by going through the tree and returning each coding for
-- each letter consisting in the tree
mkCTable :: Tree -> CTable
mkCTable t = go t [] where
	go :: Tree -> [Bit] -> CTable
	go (Leaf c _)   cd 	= [(c, cd)]
	go (Node _ l r) cd  = (go l (cd ++ [L]))  ++ (go r (cd ++ [R]))
	
-- creates the Huffman code tree, calculates each coding for each letter in this string and
-- returns a coding table which consists a coding for each letter
getCTable 	:: String -> CTable
getCTable str = mkCTable tree where
	tree   = mkTree ftable
	ftable = mkFTable str

{- Convert one bit to one byte, i.e. the zero/one bit becomes the zero/one byte. -}
bitToByte :: Bit -> Byte
bitToByte L = 0
bitToByte R = 1

{- Convert one byte to a list of exactly 8 bits. The bits should be in LSB order. -}
byteToBits :: Byte -> [Bit]
byteToBits byte = map (\ n -> if testBit byte n then R else L) [0..7]

{- Convert a list of up to 8 bits to one byte. The bits should be in LSB order. -}
bitsToByte :: [Bit] -> Byte
bitsToByte = foldl (.|.) 0 . zipWith (flip shift) [0..7] . map bitToByte
-- Alternativ: bitsToByte bits = sum (zipWith (*) [1,2,4,8,16,32,64,128] (map bitToByte bits))

{- Split a list of bits into a list of lists of bits, where each list has a
   a length of 8 (except the last one). "Data.List.Split" might be helpful. -}
splitBits :: [Bit] -> [[Bit]]
splitBits = chunksOf 8

{- Convert a list of bits to a list of bytes, so that it can be safely converted back. -}
bitsToBytes :: [Bit] -> [Byte]
bitsToBytes = map bitsToByte . splitBits . (++ [R])

{- Return the input list until (but not including) the last "R" bit. -}
dropTail :: [Bit] -> [Bit]
dropTail = go [] where
	go acc []       = []
	go acc (R : xs) = acc ++ go [R] xs
	go acc (L : xs) = go (acc ++ [L]) xs
-- Alternativ: dropTail bits = reverse (tail (dropWhile (== L) (reverse bits)))

{- Convert a list of bytes (as created by the "bitsToBytes" function) to a list of bits. -}
bytesToBits :: [Byte] -> [Bit]
bytesToBits = dropTail . concat . map byteToBits

{- QuickCheck Test that tests that bytesToBits and bitsToBytes are well defined -}
propBitsAndBytes :: [Bit] -> Bool
propBitsAndBytes xs = xs == bytesToBits (bitsToBytes xs)