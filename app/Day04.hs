module Day04 where

import Data.Maybe
import Data.List(unfoldr, groupBy)

-- Solution

{-  Splits an integer in to an array of it's digits.
    i.e. 123 -> [1,2,3]   -}
digits :: Int -> [Int]
digits = reverse . unfoldr nextDigit 
  where
    nextDigit :: Int -> Maybe (Int, Int)
    nextDigit 0 = Nothing
    nextDigit n = Just (n `mod` 10, n `div` 10)

{-  Tests whether the specified array has two adjacent
    items that are equal to each other
    e.g.  [1,1,1,2,3] returns True
          [1,2,3,4,5] returns False -}
hasAdjacentDigits :: [Int] -> Bool
hasAdjacentDigits l = any (\g -> length g >= 2) $ groupBy (==) l

{-  Tests whether the specified array has two adjacent
    items that are equal to each other, but not contained within 
    a larger group of items with the same value
    e.g.  [1,1,1,2,2] returns True 
          [1,1,1,2,2,2] returns False -}
hasAdjacentDigitsInNoLargerGroup :: [Int] -> Bool
hasAdjacentDigitsInNoLargerGroup l = any (\g -> length g == 2) $ groupBy (==) l

{-  Tests whether the specified array of digits is 
    monotonically increasing  -}
hasIncreasingDigits :: [Int] -> Bool
hasIncreasingDigits l = (length l) ==  (length $ groupBy (>) l)

{-  Given an integer, tests whether the list of digits that 
    represent that integer satisfy all the specified contraints -}
satisfies :: [[Int] -> Bool] -> Int -> Bool
satisfies predicates n = all (\f -> f nDigits) predicates
    where 
      nDigits = digits n

partOne :: [Int] -> Int
partOne = length . filter (satisfies [hasAdjacentDigits, hasIncreasingDigits]) 

partTwo :: [Int] -> Int
partTwo = length . filter (satisfies [hasAdjacentDigitsInNoLargerGroup, hasIncreasingDigits]) 

main :: IO ()
main = do
    putStrLn $ "Part One: "  ++ (show $ partOne [125730..579381])
    putStrLn $ "Part Two: "  ++ (show $ partTwo [125730..579381])