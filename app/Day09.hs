module Day09 where

import Advent.Input
import Advent.Intcode

runWithInput :: Int -> State -> Int
runWithInput input s = output
  where (Halt [output]) = run s [input]

partOne :: State -> Int
partOne = runWithInput 1

partTwo :: State -> Int
partTwo = runWithInput 2

main :: IO ()
main = do
  Right parsedInput <- fmap (parse pProgram) $ readDay 9
  putStrLn $ "Part One: "  ++ (show $ partOne parsedInput)
  putStrLn $ "Part Two: "  ++ (show $ partTwo parsedInput)