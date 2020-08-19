module Lib
    ( parse
    ) where

import qualified Data.Text as T
import Data.Char
import Text.Read
import Data.Maybe
import Parser

-- Naive parsing
textwrap :: (T.Text -> T.Text) -> String -> String 
textwrap f a = T.unpack ( f (T.pack a))

parseTokenNaive :: String -> String
parseTokenNaive a = T.unpack (T.unwords (T.splitOn (T.pack "+") (T.pack a)))

parseToken :: (T.Text -> T.Text) -> [Magma]-> T.Text -> Double
parseToken f groups input
  | T.null input = 0
  | isTextNumber input == True = read (T.unpack input) :: Double
  | otherwise                  = foldr1
                                 (operator (last groups))
                                 -- (
                                 --   identity (last groups)
                                 -- )
                                 (map ((\x -> parseToken f (init groups) x)) [T.strip x | x <- (T.splitOn (operatorString (last groups)) input), not (T.null x)])

isTextAlpha :: T.Text -> Bool
isTextAlpha string = foldl (&&) True (map isAlpha (T.unpack string))

isTextNumber :: T.Text -> Bool
isTextNumber string = isJust (readMaybe (T.unpack string) :: Maybe Double)

addParse :: T.Text -> T.Text -> T.Text
addParse x y = T.pack (show (((read (T.unpack x) :: Double) + (read (T.unpack y) :: Double))))

operations = [Magma 1 (**) (T.pack "^"),
              Magma 1 (*) (T.pack "*"),
              Magma 1 (/) (T.pack "/"),
              Magma 0 (+) (T.pack "+"),
              Magma 0 (-) (T.pack "-")]

data Magma = Magma { identity :: Double,
                     operator :: Double -> Double -> Double,
                     operatorString :: T.Text}

-- Not implementing
symbolStart :: String -> Bool
symbolStart x = False

parse :: [Double] -> String -> ([Double], String)
parse previous input
  | (not (previous == []) && symbolStart input) = do
      let x = (parseToken (\x -> x) operations (T.pack input))
      ([x] ++ previous, show (x + (head previous)))
  | otherwise = do
      let x = (parseToken (\x -> x) operations (T.pack input))
      ([x] ++ previous, show x)
      

