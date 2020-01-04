module Day15 where

import Advent.Input
import Advent.Intcode
import Data.Maybe
import Algorithm.Search(dijkstra)
import Data.Set as S
import Data.List as L

type Position = (Int, Int)
data PathState = PathState {
    exe :: Exe
  , position :: Position
  , lastReply :: Int
  }

instance Eq PathState where
  (PathState _ p1 _) == (PathState _ p2 _) = p1 == p2

instance Ord PathState where
  compare (PathState _ p1 _) (PathState _ p2 _) = compare p1 p2

instance Show PathState where
  show (PathState _ pos _) = show pos

move :: Int -> Position -> Position
move 1 (x, y) = (x, y + 1) -- North
move 2 (x, y) = (x, y - 1) -- South
move 3 (x, y) = (x - 1, y) -- East
move 4 (x, y) = (x + 1, y) -- West

possibleMoves :: PathState -> [PathState]
possibleMoves (PathState exe pos _) = 
  catMaybes $
  fmap newPathState $ 
  [1..4] -- [N, S, E, W]
  where
    newPathState i 
      | output == 0 = Nothing -- Hit a wall
      | otherwise = Just (PathState exe' newPos output)
      where 
        (Waiting exe' [output]) = exe [i]
        newPos = if output == 0 then pos else move i pos

shortestPathToOxygen :: State -> Maybe (Int, [PathState])
shortestPathToOxygen program = dijkstra possibleMoves cost reached initial
  where 
    cost _ _ = 1
    reached = (==) 2 . lastReply
    exe = run program
    initial = PathState exe (0,0) 1

partOne :: State -> Int
partOne program = pathLength
  where 
    Just (pathLength, _ ) = shortestPathToOxygen program

data FillState = FillState {
    filled :: Set PathState

    {-  Those states that represent locations that were filled
        with oxygen in the last minute. -}
  , newlyFilled :: Set PathState 
  }

{-  Generates the next element of the sequence as long
    as new locations are being filled.
    
    Passing the parameter "lastFilled" is an optimisation to
    ensure we only fill new regions with oxygen from the most
    recently filled regions.  -}
tick :: FillState -> Maybe (Int, FillState)
tick (FillState filled lastFilled)
  | S.null newlyFilled = Nothing
  | otherwise = Just $ (i, fillState)
    where 
      justFilled = 
        S.fromList $ 
        concat $ 
        fmap possibleMoves $ 
        S.toList lastFilled

      newlyFilled = S.difference justFilled filled
      newFilled = S.union justFilled filled

      fillState = FillState newFilled newlyFilled
      i = length newlyFilled

partTwo :: State -> Int
partTwo program = L.length $ L.unfoldr tick $ FillState filled filled
  where 
    Just (_, path) = shortestPathToOxygen program
    oxygenStationState = last path
    filled = S.fromList [oxygenStationState]

main :: IO ()
main = do
  Right parsedInput <- fmap (parse pProgram) $ readDay 15
  putStrLn $ "Part One: " ++ (show $ partOne parsedInput)
  putStrLn $ "Part Two: " ++ (show $ partTwo parsedInput)