module Day05 where

import Advent.Input
import Advent.Intcode
import Data.Map as M

partOne :: State -> Maybe Int
partOne p = execute operations1 $ p { inputs = [1] }
  where
    operations1 = M.fromList
        [ (1, add)
        , (2, multiply)
        , (3, readInput)
        , (4, pushOutput)
        ]

partTwo :: State -> Maybe Int
partTwo p = output $ executeFull $ p { inputs = [5] }

main :: IO ()
main = do
    parsedInput <- fmap (parse pProgram) $ readDay 5
    let (Right input) = parsedInput
    let parsedState = input
    putStrLn $ "Part One: "  ++ (show $ partOne parsedState)
    putStrLn $ "Part Two: "  ++ (show $ partTwo parsedState)