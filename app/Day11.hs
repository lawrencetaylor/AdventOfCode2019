module Day11 where

import Advent.Input
import Advent.Intcode
import Data.Maybe
import Data.List
import Data.Map as M
import Debug.Trace

type Position = (Int,Int)
type Colour = Int
type Turn = Int
data Direction = N | E | S | W deriving (Show)

data RobotState = RobotState {
    current :: (Position, Direction)
  , grid :: Map Position Colour
  , next :: Colour -> Result
  }

instance Show RobotState where
  show s = 
    concat
    $ intersperse "\n" 
    $ reverse 
    $ fmap (\y -> [ toChar $ fromMaybe 0 $ M.lookup (x,y) colours | x <- [minX..maxX] ]) [minY..maxY]
    where 
      colours = grid s
      coords = M.keys $ grid s
      minX = minimum $ fmap fst $ coords
      maxX = maximum $ fmap fst $ coords
      minY = minimum $ fmap snd $ coords
      maxY = maximum $ fmap snd $ coords

      toChar 0 = ' '
      toChar 1 = 'X'

newDirection :: Direction -> Turn -> Direction
newDirection N 0 = W
newDirection E 0 = N
newDirection S 0 = E
newDirection W 0 = S
newDirection N 1 = E
newDirection E 1 = S
newDirection S 1 = W
newDirection W 1 = N

move :: Position -> Direction -> Position
move (x,y) N = (x, y+1)
move (x,y) E = (x+1, y)
move (x,y) S = (x, y-1)
move (x,y) W = (x-1, y)

type SequenceSelector a = RobotState -> RobotState -> a

nextStep :: SequenceSelector a -> RobotState -> Maybe (a, RobotState)
nextStep selector s = 
  case (nextS currentColour) of
  Halt output -> Nothing
  Waiting exe output -> 
    let 
      [c, t] = output
      newGrid = M.insertWith (\n o -> n) pos c colours -- colour current panel
      newD = newDirection d t -- determine new direction
      newP = move pos newD    -- determine new position
      newS = RobotState (newP, newD) newGrid (\col -> exe [col])
    in Just (selector s newS, newS)
  where 
    (pos, d) = current s
    colours = grid s
    currentColour = fromMaybe 0 $ M.lookup pos colours 
    nextS = next s

partOne s = 
  length . nub 
  $ unfoldr (nextStep oldPosition) (RobotState ((0,0), N) M.empty (\c -> run s [c]))
  where 
    oldPosition oldState newState  = fst . current $ oldState

partTwo s = 
  head
  $ reverse
  $ unfoldr (nextStep oldPosition) (RobotState ((0,0), N) (M.fromList [((0,0), 1)] )  (\c -> run s [c]))
  where 
    oldPosition oldState newState  = newState

main :: IO ()
main = do
  parsedInput <- readDay 11
  let (Right program) = parse pProgram parsedInput
  putStrLn $ "Part One: " ++ (show $ partOne program)
  putStrLn $ "Part Two: \n"  ++ (show $ partTwo program)