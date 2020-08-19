module Main where

import Lib
import System.IO


-- Naive parser
main :: IO()
main = do putStr ">>> "
          hFlush stdout
          line <- getLine
          putStrLn (snd (parse [] line))
          main

