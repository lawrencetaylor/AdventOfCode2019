module Advent.Number where

import Data.List(unfoldr)

{-  Splits an integer in to an array of it's digits.
    i.e. 123 -> [1,2,3]   -}
digits :: Int -> [Int]
digits = reverse . unfoldr nextDigit 
  where
    nextDigit :: Int -> Maybe (Int, Int)
    nextDigit 0 = Nothing
    nextDigit n = Just (n `mod` 10, n `div` 10)