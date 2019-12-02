module Advent.Input where

import Data.Char as C
import Text.Parsec as P
import Text.ParserCombinators.Parsec(Parser, ParseError)

readDay :: Int -> IO String
readDay day = readFile $ dayFileName day

readDayLines :: Int -> IO [String]
readDayLines = fmap lines . readDay

dayFileName :: Int -> String
dayFileName day
  | day <= 10 = "inputs/day0" ++ (show day)
  | otherwise = "inputs/day" ++ (show day)

-- Parsing

parse :: Parser a -> String -> Either ParseError a
parse parser = P.parse parser []

pDigits :: Parser [Int]
pDigits = (fmap . fmap) C.digitToInt $ P.many1 P.digit

arrayToInt :: [Int] -> Int
arrayToInt = foldl (\a b -> b + 10*a) 0

pInt :: Parser Int
pInt = fmap arrayToInt $ pDigits