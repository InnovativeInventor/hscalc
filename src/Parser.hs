module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.Parsec.Error
import Data.Either
import Data.Maybe
import Data.Typeable


-- parser :: [Double] -> String -> ([Double], String)
-- parser = 

-- isDouble :: String -> Bool
-- isDouble input = isJust (readMaybe input :: Maybe Double)

integer :: GenParser Char st Int
integer = do
  x <- many1 digit
  return (read x :: Int)

numberDot :: GenParser Char st Double
numberDot = do
  before <- integer
  _ <- delimit "."
  after <- integer
  let result = read ((show before) ++ "." ++ (show after)) :: Double
  return result

integerDouble :: GenParser Char st Double
integerDouble = do
  x <- integer
  return (read (show x) :: Double)

number :: GenParser Char st Double
number = do 
  x <- try numberDot <|> integerDouble
  return x

delimit :: String -> GenParser Char st String
delimit symbol = do
  spaces
  _ <- try (string symbol)
  spaces
  return symbol

infixOperator :: String -> (Double -> Double -> Double) -> GenParser Char st Double
infixOperator operator operation = do
  first <- number
  _ <- delimit operator
  second <- number
  return (operation first second)

parens :: GenParser Char st Double
parens = do
  delimit "("
  string <- try (expression) <|> try (integerDouble)
  delimit ")"
  return string

expression :: GenParser Char st Double
expression = do
  expr <- try (parens) <|> try (infixOperator "*" (*)) <|> try (infixOperator "+" (+)) <|> try (infixOperator "-" (-))
  return expr

makeMaybe :: a -> Maybe a
-- makeMaybe :: Double -> Maybe Double
makeMaybe b = Just b

-- makeNothing :: a -> Maybe a
makeNothing :: a -> Maybe a
makeNothing a =  Nothing

-- type MaybeDouble = Maybe Double
-- -- isNumber :: GenParser Char st (Maybe Double)
-- isNumber :: String -> MaybeDouble
-- isNumber input = do 
--   -- x <- try numberDot <|> integerDouble
--   x <- optionMaybe parse isNumber input
--   return parse x

-- type PrimitiveType = Double
-- isPrimitive :: GenParser Char st PrimitiveType -> String -> Bool
-- isPrimitive primitive input = isJust (either (\x -> Nothing) (\x -> Just x) (parse (optionMaybe primitive) "Error!" input))
falsity :: a -> Bool
falsity a = False

panic :: a -> String
panic a = "Error!"

recursiveCall :: Double -> String
recursiveCall x = show x

parser :: String -> String
parser input
  -- | isPrimitive numberDot input  = show (parse number "Error!" input)
  | either falsity (\x -> isJust x) (parse (optionMaybe integer) "" input) == True = either panic (\x -> show x) (parse number "Error!" input)
  | either falsity (\x -> isJust x) (parse (optionMaybe numberDot) "" input) == True = either panic (\x -> show x) (parse number "Error!" input)
  | otherwise = either panic recursiveCall (parse expression "Error!" input)


-- addition :: GenParser Char st Double
-- addition = do
--   first <- number
--   delimit "+"
--   second <- number
--   return (first + second)

-- subtraction :: GenParser Char st Double
-- subtraction = do
--   first <- number
--   delimit "-"
--   second <- number
--   return (first + second)

