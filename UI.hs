module UI where

import Kaffeerunde
import System.Random

result :: Alpha -> IO ()
result a = do
	gs <- getGens
	putStrLn ("3.0")
	step(kaf (emptyData a) gs)

step :: Data -> IO ()
step (a, b, c) = do
	gs <- getGens
	putStrLn (show(head b))
	if ((length b) < 100)
		then do 
			step(kaf (a, b, c) gs)
		else putStrLn ("Done")

av :: Avgs -> Avg
av a = (sum a) / fromIntegral (length a)

readAlpha :: IO (Alpha)
readAlpha = do
	putStrLn "Please enter the threshold: "
	al <- getLine
	let parsedAl = (read al)::Alpha
	return parsedAl

getGens :: IO (Gens)
getGens = do
	g1 <- newStdGen
	g2 <- newStdGen
	g3 <- newStdGen
	return (g1, g2, g3)