module Day07 where

import Advent.Input
import Advent.Intcode(State, run, pProgram, Exe, Result(..))
import Data.List(permutations)
import Data.Maybe

type Setting = Int

initialise :: State -> [Setting] -> Int -> ([Int], [Maybe Exe])
initialise p [a,b,c,d,e] input = (oE, [eA, eB, eC, eD, eE])
  where 
    (eA, oA) = output $ run p [a, input]
    (eB, oB) = output $ run p $ [b] ++ oA
    (eC, oC) = output $ run p $ [c] ++ oB
    (eD, oD) = output $ run p $ [d] ++ oC
    (eE, oE) = output $ run p $ [e] ++ oD

    output :: Result -> (Maybe Exe, [Int])
    output (Waiting e o) = (Just e, o)
    output (Halt o) = (Nothing, o)

runPhasesWithInput :: [Exe] -> [Int] -> ([Int], [Maybe Exe])
runPhasesWithInput phases input = go phases input []
  where 
    go :: [Exe] -> [Int] -> [Maybe Exe] -> ([Int], [Maybe Exe])
    go [] output accExe = (output, reverse accExe)
    go (x:xs) input accExe = go xs newOutput (nextExe:accExe)
      where 
        (newOutput, nextExe) = nextPhase x input

    nextPhase :: Exe -> [Int] -> ([Int], Maybe Exe)
    nextPhase x input = 
      case x input of
      (Waiting x' output) -> (output, Just x')
      (Halt  output) -> (output, Nothing)

partOne :: State -> Int
partOne state = 
  maximum 
  $ fmap initialOutput
  $ permutations [0..4]
  where 
    initialOutput settings = head $ fst $ initialise state settings 0

thrustSignal :: State -> [Setting] -> Int
thrustSignal state settings = go o c
  where 
    (o, maybeC) = initialise state settings 0
    Just c = traverse id maybeC

    go :: [Int] -> [Exe] -> Int
    go input phases =
      let  
        ([output], newMaybePhases) = runPhasesWithInput phases input
      in 
        case traverse id newMaybePhases of
        Just newPhases -> go [output] newPhases
        Nothing -> output
    
partTwo :: State -> Int
partTwo state = 
  maximum 
  $ fmap (thrustSignal state)
  $ permutations [5..9]

main :: IO ()
main = do
    Right parsedInput <- fmap (parse pProgram) $ readDay 7
    putStrLn $ "Part One: "  ++ (show $ partOne parsedInput)
    putStrLn $ "Part Two: "  ++ (show $ partTwo parsedInput)