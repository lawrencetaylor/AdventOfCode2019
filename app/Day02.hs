module Day02 where

import Advent.Input
import Data.Map
import Data.Either
import Text.ParserCombinators.Parsec(Parser)
import qualified Text.Parsec as P(char, sepBy)

{- Represents the current state of the program -}
data State = State 
  { register :: Map Int Int
  , instructionPointer :: Int } 
  deriving Show

-- Parsing

pProgram :: Parser State
pProgram = fmap stateFromIntArray $ P.sepBy pInt $ P.char ','
  where
    stateFromIntArray :: [Int] -> State
    stateFromIntArray input = State register 0
      where register = fromList $ zip [0..] input

-- Solution

add :: State -> State
add s@(State reg pos) = 
  let 
    v1 = reg ! ( reg ! (pos + 1) )
    v2 = reg ! ( reg ! (pos + 2) )
    v = v1 + v2
  in s  { register = insert (reg ! (pos + 3)) v reg
        , instructionPointer = pos + 4 }

multiply :: State -> State
multiply s@(State reg pos) = 
  let 
    v1 = reg ! ( reg ! (pos + 1) )
    v2 = reg ! ( reg ! (pos + 2) )
    v = v1 * v2
  in s  { register = insert (reg ! (pos + 3)) v reg
        , instructionPointer = pos + 4 }

{-  Runs the specified program to completion, 
    returning the value in register 0 after the program 
    has halted. -}
run :: State -> Int
run s = 
  case execute s of
  Nothing -> register s ! 0
  Just newState -> run newState
  where 
    execute :: State -> Maybe State
    execute s@(State reg pos) = 
      case reg ! pos of
        1 -> Just $ add s
        2 -> Just $ multiply s
        99 -> Nothing

{-  Finds the values of the 0-register after running the 
    program until it halts with specified pair of inputs  -}
runWithInputs :: State -> (Int, Int) -> Int
runWithInputs s@(State reg _) (i1, i2) = 
  run $ s { register = insert 1 i1 $ insert 2 i2 $ reg }

{-  Generates list of all pairs of natural numbers -}
allPairs :: [(Int, Int)]
allPairs = iterate nextPair (0,0)
  where 
    nextPair :: (Int, Int) -> (Int, Int)
    nextPair (0, m) = (m+1, 0)
    nextPair (n, m) = (n-1, m+1)

{-  For inputs (12, 2), find the value in register 0 after the 
    after the program halts.  -}
partOne :: State -> Int
partOne s = runWithInputs s (12, 2)

{-  Determine the value 100*noun + verb for the input
    (noun, verb) that yields the value 19690720 in register 0
    once the program halts  -}
partTwo :: State -> Int
partTwo s = inner s allPairs
  where 
    inner :: State -> [(Int, Int)] -> Int
    inner s ((noun, verb):xs) = 
      case runWithInputs s (noun, verb) of
      19690720 -> 100*noun + verb
      _ -> inner s xs

main :: IO ()
main = do
    parsedInput <- fmap (parse pProgram) $ readDay 2
    let (Right input) = parsedInput
    let parsedState = input
    putStrLn $ "Part One: "  ++ (show $ partOne parsedState)
    putStrLn $ "Part Two: "  ++ (show $ partTwo parsedState)