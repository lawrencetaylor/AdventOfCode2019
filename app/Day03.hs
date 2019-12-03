module Day03 where

import Advent.Input
import Text.ParserCombinators.Parsec(Parser)
import qualified Text.Parsec as P(choice, char, sepBy)
import Data.Either(fromRight)
import Data.Set as S(Set, insert, empty, fromList, intersection, toList)
import qualified Data.Map as M
import Data.List(sortOn, (!!))

data Direction = N | E | S | W deriving (Show)
type Move = (Direction, Int)
type Path  = [Move]
type Point = (Int, Int)

data PathState = PathState {
  current :: Point
, stepCount :: Int
, seen :: M.Map Point Int
} deriving (Show)

initialState :: PathState
initialState = PathState (0,0) 0 M.empty

-- Parsing

pMove :: Parser Move
pMove = P.choice
  [ P.char 'U'>> ((,) N <$> pInt) 
  , P.char 'R'>> ((,) E <$> pInt)
  , P.char 'D'>> ((,) S <$> pInt)
  , P.char 'L'>> ((,) W <$> pInt) ]

pPath :: Parser Path
pPath = P.sepBy  pMove (P.char ',')

-- Solution

{-  Given a Move and a starting currentition, generates the 
    sequence of currentitions by executing the Move -}
lineSegment :: Move -> Point -> [Point]
lineSegment (N, d) (x, y) = fmap (\i -> (x, y+i)) [1..d]
lineSegment (E, d) (x, y) = fmap (\i -> (x+i, y)) [1..d]
lineSegment (S, d) (x, y) = fmap (\i -> (x, y-i)) [1..d]
lineSegment (W, d) (x, y) = fmap (\i -> (x-i, y)) [1..d]

{-  Create a new state on visiting the specified Point  
    The first argument of M.insertWith is a function that replaces
    the exising value in the event the key already exists.  
    Because we only care about the number of steps it takes to
    first reach the point.  -}
pointState :: PathState -> Point -> PathState
pointState s p = s {
    current = p
  , stepCount = newStepCount
  , seen = M.insertWith (\new old -> old) p newStepCount $ seen s
  }
  where 
    newStepCount = stepCount s + 1

{-  Creates a new state based on the specified Move.
    We fold pointState over the all the relevant Points
    in this move. -}
moveState :: Move -> PathState -> PathState
moveState move state = 
  foldl pointState state 
  $ lineSegment move (current state)

{-  Create new state based on the specified Path  -}
pathState :: PathState -> Path -> PathState
pathState s [] = s
pathState s (x:xs) = pathState (moveState x s) xs

manhattan :: (Int, Int) -> Int
manhattan (a, b) = abs a + abs b

{-  The minimum manhattan distance from (0,0) of the 
    insersection points of the two paths. -}
partOne :: [Path] -> Int
partOne moves = 
    minimum 
    $ fmap manhattan 
    $ toList $ S.intersection path1 path2
  where 
    path1 = visitedPointsM $ (moves !! 0)
    path2 = visitedPointsM $ (moves !! 1)

    visitedPointsM :: [Move] -> Set (Int, Int)
    visitedPointsM = 
      S.fromList 
      . M.keys 
      . seen 
      . pathState initialState

{-  The distance from (0,0) of the insersection points of 
    the two paths, where distance of a point p is the sum of
      * minimum number of steps required along path 1 to reach p
      * minimum number of steps required along path 2 to reach p. -}
partTwo :: [[Move]] -> Int
partTwo moves =  minimum $ fmap (\p -> (d1 M.! p) + (d2 M.! p)) $ commonPoints 
  where 
    d1 = pointsWithDistance $ (moves !! 0)
    d2 = pointsWithDistance $ (moves !! 1)
    v1 = S.fromList $ M.keys d1 
    v2 = S.fromList $ M.keys d2
    commonPoints = S.toList $ S.intersection v1 v2

    pointsWithDistance :: [Move] -> M.Map Point Int
    pointsWithDistance = seen . pathState initialState

main :: IO ()
main = do
    parsedInput <- fromRight [] <$> traverse id <$> fmap (parse pPath)<$>  readDayLines 3
    putStrLn $ "Part One: "  ++ (show $ partOne parsedInput)
    putStrLn $ "Part Two: "  ++ (show $ partTwo parsedInput)