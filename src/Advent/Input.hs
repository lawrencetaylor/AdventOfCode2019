module Advent.Input where

readDay :: Int -> IO String
readDay day = readFile $ dayFileName day

readDayLines :: Int -> IO [String]
readDayLines = fmap lines . readDay

dayFileName :: Int -> String
dayFileName day
  | day <= 10 = "inputs/day0" ++ (show day)
  | otherwise = "inputs/day" ++ (show day)

