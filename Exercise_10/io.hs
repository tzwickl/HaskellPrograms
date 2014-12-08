getName :: String -> IO String
getName message = do
	putStrLn message
	name <- getLine
	return name

greet :: String -> IO ()
greet name = do
	putStrLn ("Hello " ++ name ++ "!\n")
	putStrLn "How old are you?"
	age <- getLine
	putStrLn "How do you feel?"
	mood <- getLine
	putStrLn ("Oh, at the age of " ++ age ++ " you should feel " ++ mood ++ "!\n")
	return ()

main :: IO ()
main = do
	name <- getName "Hi! What's your name?"
	if name == "Alex" then
		do
			greet "Master"
	else
		do
			greet name
	return ()
