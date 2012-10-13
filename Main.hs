module Main where

import UI
import Kaffeerunde
import System.IO

main = do
	a <- readAlpha
	result (a)