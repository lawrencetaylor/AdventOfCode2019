module Day13 where

import Advent.Input
import Advent.Intcode
import Data.Maybe
import Data.Map(Map)
import Data.Map as M
import Data.List as L
import Data.List.Split
import Safe

data CellValue = Wall | Block | Paddle | Ball deriving (Eq)
type Cell = (Int,Int)

paddleCell :: [[Int]] -> Maybe Cell
paddleCell = 
  fmap (\[x,y,3] -> (x,y)) 
  . headMay 
  . L.filter (\[_,_,t] -> t == 3)

ballCell :: [[Int]] -> Maybe Cell
ballCell = 
  fmap (\[x,y,4] -> (x,y)) 
  . headMay 
  . L.filter (\[_,_,t] -> t == 4)

getScore :: [[Int]] -> Maybe Int
getScore =
  fmap (\[x,y,s] -> s) 
  . headMay
  . L.filter (\[x,y,_] -> (x,y) == (-1,0))

play :: Result -> Int -> Cell -> Cell -> Int
play r score ball paddle = 
  case r of
  Halt outputs -> newScore 
    where 
      chunked = chunksOf 3 outputs
      newScore = fromMaybe score $ getScore chunked
  Waiting exe outputs -> play newResult newScore newBall newPaddle
    where
      chunked = chunksOf 3 outputs
      newScore = fromMaybe score $ getScore chunked
      newBall@(ballX, _) = fromMaybe ball $ ballCell chunked
      newPaddle@(paddleX, _) = fromMaybe paddle $ paddleCell chunked
      newResult = 
        if (ballX < paddleX ) then exe [-1]
        else if (ballX > paddleX) then exe [1]
        else exe [0]

partOne :: State -> Int
partOne s = 
  length
  $ L.filter (\[_,_,t] -> t == 2)
  $ chunksOf 3 output
  where 
    Halt output = run s []

partTwo :: State -> Int
partTwo s = play (exe [0]) 0 ball paddle
  where
    playingForFree = s { register = M.insertWith (\n o -> n) 0 2 $ register s}
    Waiting exe output = run playingForFree []
    outputChunks = chunksOf 3 output

    Just paddle = paddleCell outputChunks
    Just ball = ballCell outputChunks

main :: IO ()
main = do
  parsedInput <- readDay 13
  let (Right program) = parse pProgram parsedInput
  putStrLn $ "Part One: " ++ (show $ partOne program)
  putStrLn $ "Part Two: " ++ (show $ partTwo program)