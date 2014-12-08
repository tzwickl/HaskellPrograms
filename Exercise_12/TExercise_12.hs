module TExercise_12 where

import qualified Huffman as H
import qualified Data.ByteString as BS
import qualified Data.Binary as BI
import System.IO
import System.FilePath
import System.Environment (getArgs)

-- compresses the String with the Huffman Algorithmus and writes the result to the
-- file file.huff under the passed file path
-- furthermore there is also a file file.code added to the file path where the Huffman code
-- tree is saved to decompress the coding
compress :: String -> FilePath -> IO ()
compress str fp = do
				if (isValid fp) && (not (null str)) then do
					let tree = H.getTree str
					let bits = H.encode tree str
					let binary = H.serializeBits bits
					let serial = H.serializeTree tree
					BS.writeFile (fp ++ ".code") serial
					BS.writeFile (fp ++ ".huff") binary
				else if isValid fp then do
						hPutStrLn stdout ("Invalid string: " ++ str)
					 else do
						hPutStrLn stdout ("Invalid path: " ++ fp)
					

-- reads at first the file file.code residing under the passed file path and creates the
-- Huffman code tree from it
-- afterwards the byte code from the file file.huff is read and by the Huffman 
-- decode algorithm decompressed and returned
-- if the decompression wasn't successful or a there wasn't found valid file under the 
-- passed file path Nothing is returned
decompress :: FilePath -> IO (Maybe String)
decompress fp = do
				if isValid fp then do
					rawHuffmanTree        <- BS.readFile (fp ++ ".code")
					rawString             <- BS.readFile (fp ++ ".huff")
					let tree = H.deserializeTree rawHuffmanTree
					let bits = H.deserializeBits rawString
					let str = H.decode tree bits
					return (Just str)
				else do
					hPutStrLn stdout ("Invalid path: " ++ fp)
					return Nothing
			
-- main method of the decompressing/compressing algorithm
-- the first command line argument defines the mode (c -> compression, d -> decompression)
-- the second command line argument defines the path to the file to be compressed
-- if no arguments are passed a help text will be printed
main :: IO ()
main = do
		args <- getArgs
		case (length args) of
			0 -> hPutStrLn stdout (helpText "Invalid number of arguments!!")
			1 -> hPutStrLn stdout (helpText "Invalid number of arguments!!")
			2 -> do
				let filePath = head $ tail args
				case (head args) of
					"c" -> do
						fileHandle <- openFile filePath ReadMode
						content <- hGetContents fileHandle
						let filePathWE = dropExtension filePath
						compress content filePathWE
						hClose fileHandle
					"d" -> do
						let filePathWE = dropExtension filePath
						str <- decompress filePathWE
						let content = fromJust str
						fileHandle <- openFile filePath WriteMode
						hPutStr fileHandle content
						hClose fileHandle where
							fromJust (Just x)  = x
							fromJust (Nothing) = "Error while decompressing file"
					_   -> hPutStrLn stdout (helpText "Invalid first argument!!")

-- returns a help text which is shown on the console to inform the user that an invalid
-- input has been made
helpText :: String -> String
helpText str = (str ++ "\nTo compress a file" 
				++ " please enter c followed by the file path and the name"
				++ " of the file to be compressed!!\n"
				++ "To decompress a file please enter d followed by the"
				++ " file path which consists of the two files (.huff)"
				++ " and (.code)!!");
		