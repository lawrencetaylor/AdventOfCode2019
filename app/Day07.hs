module Day07 where

import Advent.Input
import Advent.Intcode as C(State(..), executeFull, pProgram)
import Data.List(permutations)

data Phase = Phase {
    setting :: Int
  }

type Setting = Int

runPhase :: State -> Setting -> Int -> Int
runPhase s setting  lastOutput = finalOutput
  where
    withInput = s { inputs = [setting, lastOutput] }
    runWithInput = executeFull withInput
    (Just finalOutput) = output runWithInput

runPhases :: Int -> State -> [Int] -> Int
runPhases i state [setting] = runPhase state setting i
runPhases i state (x:xs) = runPhases nextInput state xs
  where
    nextInput = runPhase state x i
  
partOne state = maximum $ fmap (runPhases 0 state) $ permutations [0..4]

-- progStr = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
-- setting = [9,8,7,6,5]

-- run

main :: IO ()
main = do
    Right parsedInput <- fmap (parse pProgram) $ readDay 7
    putStrLn $ "Part One: "  ++ (show $ partOne parsedInput)

    -- 66666 too low

    