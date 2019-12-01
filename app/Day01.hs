module Day01 where

import Advent.Input
import Data.List

readLine :: String -> Int
readLine = read

transform :: Int -> Int
transform mass = (div mass 3) - 2
        
fuelRequirements :: Int -> Int
fuelRequirements = sum . unfoldr nextItem
  where
    nextItem :: Int -> Maybe (Int, Int)
    nextItem m = 
        case (nextM <= 0) of
        True -> Nothing
        False -> Just (nextM, nextM)
        where nextM = transform m

partOne :: [Int] -> Int
partOne = sum . fmap transform

partTwo :: [Int] -> Int
partTwo = sum . fmap fuelRequirements

main :: IO ()
main = do
    parsedInput <- fmap (fmap readLine) $ readDayLines 1
    putStrLn $ "Part One: "  ++ (show $ partOne parsedInput)
    putStrLn $ "Part Two: "  ++ (show $ partTwo parsedInput)