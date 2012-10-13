module UI where

import Kaffeerunde

result :: Alpha -> IO ()
result a = step(kaf (emptyData a))

step :: Data -> IO ()
step (a, b, c) = do
	putStrLn (show(av b))
	if ((length b) < 100)
		then step(kaf (a, b, c))
		else putStrLn ("Done")

av :: Avgs -> Avg
av a = (sum a) / fromIntegral (length a)

readAlpha :: IO (Alpha)
readAlpha = do
	putStrLn "Please enter the threshold: "
	al <- getLine
	let parsedAl = (read al)::Alpha
	return parsedAl
