module Advent.Input where

import Data.Char as C
import Text.Parsec as P
import Text.ParserCombinators.Parsec(Parser, ParseError)
import qualified Text.Parsec as P(option)
import Debug.Trace

readDay :: Int -> IO String
readDay day = readFile $ dayFileName day

readDayLines :: Int -> IO [String]
readDayLines = fmap lines . readDay

dayFileName :: Int -> String
dayFileName day
  | day < 10 = "inputs/day0" ++ (show day)
  | otherwise = "inputs/day" ++ (show day)

-- Parsing

parse :: Parser a -> String -> Either ParseError a
parse parser = P.parse parser []

pDigits :: Parser [Int]
pDigits = (fmap . fmap) C.digitToInt $ P.many1 P.digit

arrayToInt :: [Int] -> Int
arrayToInt = foldl (\a b -> b + 10*a) 0

pInt :: Parser Int
pInt = do
  multiplier <- toInt <$> (option '+' $ char '-')
  value <- fmap arrayToInt $ pDigits
  return $ multiplier * value
  where
    toInt :: Char -> Int
    toInt '+' = 1
    toInt '-' = -1

-- Logging

traceLabel :: (Show a) => String -> a -> a
traceLabel l a = trace (l ++ ": " ++ (show a)) a