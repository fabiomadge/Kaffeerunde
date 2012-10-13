module UI where

import Kaffeerunde

result :: Alpha -> IO ()
result a = do
	putStrLn (show (a))

readAlpha :: IO (Alpha)
readAlpha = do
	putStrLn "Please enter the threshold: "
	al <- getLine
	let parsedAl = (read al)::Alpha
	return parsedAl
