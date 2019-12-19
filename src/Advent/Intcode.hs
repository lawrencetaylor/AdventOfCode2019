module Advent.Intcode(
    pProgram
  , State(..)
  , Result(..)
  , Exe(..)
  , run
  , runState
  , add
  , multiply
  , readInput
  , pushOutput
  , jumpIfTrue
  , jumpIfFalse
  , lessThan
  , equals) where

import Advent.Input
import Advent.Number
import Data.Map as M
import Text.ParserCombinators.Parsec(Parser)
import qualified Text.Parsec as P(char, sepBy)

type Register = Map Int Int

{- Represents the current state of the program -}
data State = State 
  { register :: Register
  , instructionPointer :: Int
  , relativeBase :: Int } 
  deriving Show

-- Parsing

pProgram :: Parser State
pProgram = fmap stateFromIntArray $ P.sepBy pInt $ P.char ','
  where
    stateFromIntArray :: [Int] -> State
    stateFromIntArray input = State register 0 0
      where register = fromList $ zip [0..] input

-- Model 

type Mode = Int
type ParameterModes = Map Int Mode

{-  Creates a map of "Parameter Index" -> "Mode". -}
parameterModes :: Int -> ParameterModes
parameterModes = 
  fromList 
  . zip [1..] 
  . Prelude.drop 2 
  . reverse 
  . digits

(!-) :: Register -> Int -> Int
(!-) reg i = 
  case M.lookup i reg of
  Just j -> j
  Nothing -> 
    case i < 0 of
    True -> error "Negative index"
    False -> 0

address :: ParameterModes -> State -> Int -> Maybe Int
address modes s parameterIndex = 
  let 
    pos = instructionPointer s
    reg = register s
    unshiftedAddress = reg !- (pos + parameterIndex)
    rb = relativeBase s
  in
    case M.lookup parameterIndex modes of
      Just 2 -> Just $ rb + unshiftedAddress
      Just 1 -> Nothing
      _ -> Just $ unshiftedAddress  

{-  Gets the value of the the parameter, taking in to account
    the specified parameter modes.  -}
value :: ParameterModes -> Int -> State -> Int -> Int
value modes pos s i = 
  case address modes s i of
  Just add -> reg !- add
  Nothing -> reg !- (pos + i)
  where 
    reg = register s

add :: State -> State
add s = 
  let 
    reg = register s
    pos = instructionPointer s
    modes = parameterModes $ reg !- pos
    v1 = value modes pos s 1
    v2 = value modes pos s 2
    v = v1 + v2
    Just i3 = address modes s 3
    msg = "Add: Reg[" ++ (show i3) ++ "] = " ++ (show v)
  in 
    s  { register = insert i3 v reg
        , instructionPointer = pos + 4 }

multiply :: State -> State
multiply s = 
  let 
    reg = register s
    pos = instructionPointer s
    modes = parameterModes $ reg !- pos
    v1 = value modes pos s 1
    v2 = value modes pos s 2
    v = v1 * v2
    Just i3 = address modes s 3
    msg = "Multiply: Reg[" ++ (show i3) ++ "] = " ++ (show v)
  in 
    s  { register = insert i3 v reg
        , instructionPointer = pos + 4 }

readInput :: State -> Int -> State
readInput s input =
  let 
    reg = register s
    pos = instructionPointer s
    modes = parameterModes $ reg !- pos
    (Just v) = address modes s 1
    msg = "Read: " ++ (show $ input) ++ " -> Reg[" ++ (show v) ++ "]"
  in
    s { register = insert v input reg
      , instructionPointer = pos + 2 }

pushOutput :: State -> (State, Int)
pushOutput s =
  let 
    reg = register s
    pos = instructionPointer s
    modes = parameterModes $ reg !- pos
    v = value modes pos s 1
    msg = "Output: " ++ (show $ v)
  in 
    (s { instructionPointer = pos + 2}, v)

jumpIfTrue :: State -> State
jumpIfTrue s =
  let 
    reg = register s
    pos = instructionPointer s
    modes = parameterModes $ reg !- pos
    v = value modes pos s 1
    newPos = 
      case v /= 0 of
      True -> value modes pos s 2
      False -> pos + 3
    msg = "JumpIfTrue: Position -> " ++ (show newPos)
  in 
    s { instructionPointer = newPos }

jumpIfFalse :: State -> State
jumpIfFalse s =
  let 
    reg = register s
    pos = instructionPointer s
    modes = parameterModes $ reg !- pos
    v = value modes pos s 1
    newPos = 
      case v == 0 of
      True -> value modes pos s 2
      False -> pos + 3
    msg = "JumpIfFalse: Position -> " ++ (show newPos)
    
  in 
    s { instructionPointer = newPos }

lessThan :: State -> State
lessThan s =
  let 
    reg = register s
    pos = instructionPointer s
    modes = parameterModes $ reg !- pos
    v1 = value modes pos s 1
    v2 = value modes pos s 2
    v = 
      case v1 < v2 of
      True -> 1
      False -> 0
    Just i3 = address modes s 3
    msg = "LessThan: Reg[" ++ (show i3) ++ "] = " ++ (show v)
  in 
    s  { register = insert i3 v reg
        , instructionPointer = pos + 4 }

equals :: State -> State
equals s =
  let 
    reg = register s
    pos = instructionPointer s
    modes = parameterModes $ reg !- pos
    v1 = value modes pos s 1
    v2 = value modes pos s 2

    v = 
      case v1 == v2 of
      True -> 1
      False -> 0
    Just i3 = address modes s 3
    msg = "Equals: Reg[" ++ (show i3) ++ "] = " ++ (show v)
  in 
    s  { register = insert i3 v reg
        , instructionPointer = pos + 4 }

adjustBase :: State -> State
adjustBase s = 
  let 
    reg = register s
    pos = instructionPointer s
    rel = relativeBase s
    modes = parameterModes $ reg !- pos 
    v = value modes pos s 1
    msg = "AdjustBase: Base = " ++ (show $ rel + v)
  in 
    s {  relativeBase = rel + v
        , instructionPointer = pos + 2 }

data Result = 
  Waiting Exe [Int]
  | Halt [Int]

type Exe = [Int] -> Result

go :: State -> [Int] -> Exe
go s@(State reg pos relBase) accOutput input  = 
  let 
    opCode = reg !- pos
    opC = opCode `mod` 100
  in 
    case opC of
    1 -> go (add s) accOutput input 
    2 -> go (multiply s) accOutput input
    3 -> case input of
          [] -> Waiting (\i -> go s [] i) (reverse accOutput)
          (i:is) -> go (readInput s i) accOutput is
    4 -> 
      let (newState, output) = pushOutput s
      in (go newState (output : accOutput) input)
    5 -> go (jumpIfTrue s) accOutput input
    6 -> go (jumpIfFalse s) accOutput input
    7 -> go (lessThan s) accOutput input
    8 -> go (equals s) accOutput input
    9 -> go (adjustBase s) accOutput input
    99 -> Halt (reverse accOutput)

runState :: State -> Exe
runState s = go s []

run :: State -> Exe
run s = runState s 