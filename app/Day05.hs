module Day05 where

import Advent.Input
import Advent.Number
import Data.Map as M
import Data.Either
import Text.ParserCombinators.Parsec(Parser)
import qualified Text.Parsec as P(char, sepBy)

type Register = Map Int Int

{- Represents the current state of the program -}
data State = State 
  { register :: Register
  , instructionPointer :: Int
  , input :: Int
  , output :: Maybe Int } 
  deriving Show

-- Parsing

pProgram :: Parser State
pProgram = fmap stateFromIntArray $ P.sepBy pInt $ P.char ','
  where
    stateFromIntArray :: [Int] -> State
    stateFromIntArray input = State register 0 1 Nothing
      where register = fromList $ zip [0..] input

-- Solution

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

{-  Gets the value of the the parameter, taking in to account
    the specified parameter modes.  -}
value :: ParameterModes -> Int -> Register -> Int -> Int
value modes pos reg i = parameter mode p reg
  where
    p = reg ! (pos + i)
    mode = 
      case M.lookup i modes of
      Just m -> m
      Nothing -> 0

    parameter :: Mode -> Int -> Register -> Int
    parameter 0 i reg = reg ! i
    parameter 1 i _ = i

add :: State -> State
add s@(State reg pos _ _) = 
  let 
    modes = parameterModes $ reg ! pos
    v1 = value modes pos reg 1
    v2 = value modes pos reg 2
    v = v1 + v2
  in s  { register = insert (reg ! (pos + 3)) v reg
        , instructionPointer = pos + 4 }

multiply :: State -> State
multiply s@(State reg pos _ _) = 
  let 
    modes = parameterModes $ reg ! pos
    v1 = value modes pos reg 1
    v2 = value modes pos reg 2
    v = v1 * v2
  in s  { register = insert (reg ! (pos + 3)) v reg
        , instructionPointer = pos + 4 }

readInput :: State -> State
readInput s@(State reg pos i _) =
  s { register = insert (reg ! (pos + 1)) i reg
    , instructionPointer = pos + 2}

pushOutput :: State -> State
pushOutput s@(State reg pos i _) =
  let 
    modes = parameterModes $ reg ! pos
    v = value modes pos reg 1
  in 
    s { output = Just $ v
      , instructionPointer = pos + 2}

jumpIfTrue :: State -> State
jumpIfTrue s@(State reg pos i _) =
  let 
    modes = parameterModes $ reg ! pos
    v = value modes pos reg 1
    newPos = 
      case v /= 0 of
      True -> value modes pos reg 2
      False -> pos + 3
  in 
    s { instructionPointer = newPos }

jumpIfFalse :: State -> State
jumpIfFalse s@(State reg pos i _) =
  let 
    modes = parameterModes $ reg ! pos
    v = value modes pos reg 1
    newPos = 
      case v == 0 of
      True -> value modes pos reg 2
      False -> pos + 3
  in 
    s { instructionPointer = newPos }

lessThan :: State -> State
lessThan s@(State reg pos i Nothing) =
  let 
    modes = parameterModes $ reg ! pos
    v1 = value modes pos reg 1
    v2 = value modes pos reg 2
    v = 
      case v1 < v2 of
      True -> 1
      False -> 0
  in 
    s  { register = insert (reg ! (pos + 3)) v reg
        , instructionPointer = pos + 4 }

equals :: State -> State
equals s@(State reg pos i _) =
  let 
    modes = parameterModes $ reg ! pos
    v1 = value modes pos reg 1
    v2 = value modes pos reg 2
    v = 
      case v1 == v2 of
      True -> 1
      False -> 0
  in 
    s  { register = insert (reg ! (pos + 3)) v reg
        , instructionPointer = pos + 4 }

{-  Executes the specified program using the 
    mapping of OpCode -> Method.  Returns the 
    last value outputted when the program halts.  -}
execute :: Map Int (State -> State) -> State -> Maybe Int
execute operations s@(State reg pos _ out) = 
  case M.lookup code operations of
  Just f -> execute operations (f s)
  Nothing ->
    case code of
    99 -> out
    c -> error $ "Invalid Opcode: " ++ (show c)
  where 
    code = (reg ! pos) `mod` 100

partOne :: State -> Maybe Int
partOne p = execute operations1 $ p { input = 1}
  where
    operations1 = M.fromList
        [ (1, add)
        , (2, multiply)
        , (3, readInput)
        , (4, pushOutput)
        ]

partTwo :: State -> Maybe Int
partTwo p = execute operations2 $ p { input = 5}
  where
    operations2 = M.fromList
        [ (1, add)
        , (2, multiply)
        , (3, readInput)
        , (4, pushOutput)
        , (5, jumpIfTrue)
        , (6, jumpIfFalse)
        , (7, lessThan)
        , (8, equals)
        ]

main :: IO ()
main = do
    parsedInput <- fmap (parse pProgram) $ readDay 5
    let (Right input) = parsedInput
    let parsedState = input
    putStrLn $ "Part One: "  ++ (show $ partOne parsedState)
    putStrLn $ "Part Two: "  ++ (show $ partTwo parsedState)