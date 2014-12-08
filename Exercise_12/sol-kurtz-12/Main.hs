module Main where

import Data.Bits
import Data.Char
import Data.List.Split
import Data.Word
import Huffman
import qualified Data.ByteString
import Test.QuickCheck

{------------------------------------- G2 -------------------------------------}

data Html = Text String | Block String [Html]
	deriving Show

html_ex1 = Text "Every string should learn to swim"
html_ex2 = Block "head" []
html_ex3 = Block "body" [Block "p" [Text "My cat"], Text "is not a float"]
html_ex4 = Text "Sei Epsilon < 0"
html_ex5 = Text "Üblicherweise ist 𝜀 > 0"

namedEntities :: [(Char, String)]
namedEntities = [
	('<', "lt"), ('>', "gt"), ('&',"amp"), ('ß', "szlig"),
	('Ä', "Auml"), ('Ö', "Ouml"), ('Ü', "Uuml"),
	('ä', "auml"), ('ö', "ouml"), ('ü', "uuml")
	]

{- Convert one character to its HTML representation -}
htmlChar :: Char -> String
htmlChar char = case lookup char namedEntities of
    Just name -> "&" ++ name ++ ";"
    Nothing
    	| ord char < 128 -> [char]
    	| otherwise      -> "&#" ++ show (ord char) ++ ";"

{- Convert a "HTML" object as defined above to a plain string -}
plainHtml :: Html -> String
plainHtml (Text cs)    = concat (map htmlChar cs)
plainHtml (Block s hs) =
	"<" ++ s ++ ">" ++ concat (map plainHtml hs) ++ "</" ++ s ++ ">"

{------------------------------------- G1 -------------------------------------}

{- Define a suitable representation for one byte. "Data.Word" might be helpful. -}
type Byte = Word8

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

{- Convert a list of bits to a byte string -}
bitsToByteString :: [Bit] -> Data.ByteString.ByteString
bitsToByteString = Data.ByteString.pack . bitsToBytes

{- Convert a byte string (as created by the "bitsToByteString" function) to a list of bits. -}
byteStringToBits :: Data.ByteString.ByteString -> [Bit]
byteStringToBits = bytesToBits . Data.ByteString.unpack

{- QuickCheck Test that tests that bytesToBits and bitsToBytes are well defined -}
propBitsAndBytes :: [Bit] -> Bool
propBitsAndBytes xs = xs == bytesToBits (bitsToBytes xs)

{- Compress a string using Huffman coding and write the results to disk. -}
compress :: String -> FilePath -> IO ()
compress string file = do
	let frequencyTable   = mkFTable string
	let huffmanTree      = mkTree frequencyTable
	let characterTable   = mkCTable huffmanTree
	let compressedString = encode characterTable string
	let serializedTree   = serializeTree huffmanTree
	let serializedString = bitsToByteString compressedString
	Data.ByteString.writeFile (file ++ ".code") serializedTree
	Data.ByteString.writeFile (file ++ ".huff") serializedString

{- Read two compressed files from disk, decompress them and return the result. -}
decompress :: FilePath -> IO String
decompress file = do
	rawHuffmanTree        <- Data.ByteString.readFile (file ++ ".code")
	rawString             <- Data.ByteString.readFile (file ++ ".huff")
	let huffmanTree        = deserializeTree rawHuffmanTree
	let deserializedString = byteStringToBits rawString
	let decodedString      = decode huffmanTree deserializedString
	return decodedString
