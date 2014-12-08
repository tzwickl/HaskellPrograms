module Exercise_11 where
import Data.Time
import Network
import System.IO
import System.Locale
import Data.List

-- Für H11.1
import qualified AssocList as AL
import qualified Map2 as M2

{- Library -- nicht veraendern -}

vocabs = ["i", "more", "more", "now", "want", "won", "wow"]

getTime :: String -> IO String
getTime str = do
  time <- getZonedTime
  let output = formatTime defaultTimeLocale str time
  return output

{- G11.1 -}

{- Zu Lösen in eigener Datei -}

{- G11.2 -}

{- Zu Lösen in eigener Datei -}

{- G11.3 -}
-- returns either a tuple which consists of the wrong spelled word and all quasi-identical words found in the vocabular
-- or the word itself if it was correctly spelled
safeFixTypos :: [String] -> [String] -> [Either (String, [String]) String]
safeFixTypos _ [] = []
safeFixTypos voc (x:xs) = [if not (null ys) && (y == head ys) then Right x else Left (y, ys)] ++ safeFixTypos voc xs where
		(y, ys) = fixTypo voc x

-- returns wether two given words are quasi identical or not
-- two words are quasi identical if they are identical and at most one letter is allowed
-- to be different, also there isn't allowed to be more than one quasi identical word in the vocabular
quasiIdentical :: String -> String -> Bool
quasiIdentical [] [] = True
quasiIdentical (c : cs) (d : ds) = (c == d && quasiIdentical cs ds) || cs == ds
quasiIdentical _ _ = False

-- returns the word and a list of words that are quasi identical to the passed word
fixTypo :: [String] -> String -> (String, [String])
fixTypo vocabs word = (word, quasis)
  where quasis = nub (filter (quasiIdentical word) vocabs)

{- H11.1 -}

{- Teilaufgabe 1 solved in file Map2.hs -}

{- Teilaufgabe 2 -}

{- assocListToMap2 -}
-- converts a assocList to a Map2 by inserting each element from assocList into Map2
assocListToMap2 :: (Ord k, Ord v) => AL.Map k v -> M2.Map k v
assocListToMap2 map = go (AL.keys map) M2.empty where
		go [] map2 = map2
		go (x:xs) map2 = go xs map2' where
			map2' = M2.insert x el map2
			el = getValue (AL.lookup x map)
			getValue (Just x) = x

{- map2ToAssocList -}
-- converts a Map2 to a assocList by inserting each element from Map2 into assocList
map2ToAssocList :: (Ord k, Ord v) => M2.Map k v -> AL.Map k v
map2ToAssocList map = go (M2.keys map) AL.empty where
		go [] map2 = map2
		go (x:xs) map2 = go xs map2' where
			map2' = AL.insert x el map2
			el = getValue (M2.lookup x map)
			getValue (Just x) = x
			
{- Teilaufgabe 3 -}
{-
The preceding two implementations are inefficient because they are restricted to the 
functions accessible from outside of the modules. The problem is that we don't have direct access
to the internal representation of the data structure and therefore we need to use the
interface functions, what does mean that we have to conduct a lookup for each key in the 
one map and add than the value and the key to the other map. If we would have access to the
internal data structure we could directly access each element in the list/tree without the need of
iterating through all elements each time we conduct a lookup for the search of the desired key 
This makes this way of converting from one map to the other map very inefficient and slow.
To improve the efficiency of this conversion I just added a from outside accessible
function toList to each module which converts the map to a list. Through this function
we are now able to convert a map to a from the internal data representation independent list
and add each tuple in the list to the to converting map. This improves the efficiency of the
converting function in this way that we now don't need to conduct a lookup for each key, instead we
are now iterating through the list in O(n) and add each pair to the map.
-}
{- more efficient implementation of assocListToMap2 -}
-- converts a assocList to a Map2 by inserting each element from assocList into Map2
assocListToMap2' :: (Ord k, Ord v) => AL.Map k v -> M2.Map k v
assocListToMap2' map = go (AL.toList map) M2.empty where
		go [] map2 = map2
		go ((k,v):xs) map2 = go xs map2' where
			map2' = M2.insert k v map2

{- more efficient implementation of map2ToAssocList -}
-- converts a Map2 to a assocList by inserting each element from Map2 into assocList
map2ToAssocList' :: (Ord k, Ord v) => M2.Map k v -> AL.Map k v
map2ToAssocList' map = go (M2.toList map) AL.empty where
		go [] map2 = map2
		go ((k,v):xs) map2 = go xs map2' where
			map2' = AL.insert k v map2

{- H11.2 -}

{- Folgende Funktionen können Sie zur Lösung verwenden (andere Funktionen sind natürlich auch gestattet):

    -- definiert in Network
    -- erzeugt einen Socket und wartet auf eingehende Verbindungen
    -- Muss nur ein einziges Mal beim Start des Servers aufgerufen werden.
    -- Bsp.: listenOn (PortNumber 80)
    listenOn :: PortID -> IO Socket

    -- definiert in Network
    -- erstellt ein I/O-Handle für eine Verbindung
    -- Kann nach einem Protokolllauf erneut aufgerufen werden.
    -- Für die Bearbeitung der Aufgabe ist nur das Handle notwendig, die anderen Rückgabewerte können ignoriert werden.
    accept :: Socket -> IO (Handle, HostName, PortNumber)

    -- definiert in System.IO
    -- schließt ein I/O-Handle
    -- muss unbedingt nach Beendigung eines Protokolllaufs oder bei vorzeitigem Abbruch aufgerufen werden
    hClose :: Handle -> IO ()

    -- vordefiniert in dieser Datei
    -- nimmt einen Format-String und gibt Datum/Zeit als String zurück
    -- Syntax der Format-Strings:
    --   <http://hackage.haskell.org/package/time-1.4.0.1/docs/Data-Time-Format.html#v:formatTime>
    getTime :: String -> IO String

-}
-- initialise the Server to listen on port number 9002
server :: IO ()
server = withSocketsDo $ do
			socket <- listenOn (PortNumber 9002)
			loop socket

-- this is an infinite loop which is just calling the protocol and after the protocol
-- has been finished it calls itself again
loop :: Socket -> IO ()
loop s = do
		processProtocol s
		loop s

-- waits for the connection of a client on port 9002 and sends a hello message to 
-- the connected client
-- after the connection has been established the server waits till the client sends 
-- a message and evaluates the message
-- if the message was valid the client gets its desired information else the connection is closed
processProtocol :: Socket -> IO ()
processProtocol s = do 
			(h,_,_) <- accept s
			hSetBuffering h LineBuffering
			hPutStrLn h "I2B11H2P/1.0 HELLO"
			message <- hGetLine h
			let format = evaluateMessage message
			case format of
					"" -> do
						hClose h
					_  -> do
						time <- getTime format
						hPutStrLn h time
						hClose h

-- evaluates the message received from the client
-- if the message was a valid message (DATE or TIME) the respective format code is
-- returned, else an empty String is returned as an indicator that the message was invalid		
evaluateMessage :: String -> String
evaluateMessage xs 
	| isInfixOf "DATE" xs = "%F"
	| isInfixOf "TIME" xs = "%X"
	| otherwise = ""