{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Day10 where

import Advent.Input
import Data.Map(Map)
import qualified Data.Map as M
import Data.Set(Set)
import qualified Data.Set as S
import Data.List as L
import Data.Maybe

type Coord = (Int, Int)

(*#) :: Int -> Coord -> Coord
(*#) m (x,y) = (m*x, m*y)

(+#) :: Coord -> Coord -> Coord
(+#) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

(-#) :: Coord -> Coord -> Coord
(-#) a b = a +# ((-1) *# b)

origin :: Coord
origin = (0,0)

abs2 :: Coord -> Int
abs2 (x,y) = x*x + y*y 

pAsteroids :: [String] -> [Coord]
pAsteroids lines =
  fmap fst
  $ filter ((==) '#' . snd)
  $ [ ((x,y), char) |
      (line, y) <- zip lines [0..]
    , (char, x) <- zip line [0..] ]

{-  Checks if a candidate asteroid is in the line of sight of 
    the specified base. -}
isInLineOfSight :: [Coord] -> Coord -> Coord -> Bool
isInLineOfSight asteroids base candidate =  
  not
  $ any (\c -> c `elem` asteroids)
  $ takeWhile ((/=) candidate)
  $ fmap (\m -> base +# (m *# step) ) [1..] 
  where 
    step = rotClass $ candidate -# base

numberOfAsteroidsInSight :: [Coord] -> Coord -> Int
numberOfAsteroidsInSight asteroids base = 
  length
  $ filter (isInLineOfSight asteroids base)
  $ L.delete base asteroids

{-  Gets the coordinate of the asteroid that has the 
    most asteroids in it's line of sight. -}
bestLocation :: [Coord] -> Coord
bestLocation asteroids = 
  head
  $ reverse
  $ sortOn (numberOfAsteroidsInSight asteroids) 
  $ asteroids

{-  Represents and equivalence class of points on the grid
    that have the same angle around the origin wrt the x axis.-}
type RotationClass = (Int,Int)

{-  Gets the rotation equivalence class associated a coordinate.  -}
rotClass :: Coord -> RotationClass
rotClass (x,y) = (x `div` d, y `div` d)
  where 
    d = gcd x y

{-  Partitions asteroid coordinates in to equivalence classes
    represented by their angle of rotation in polar coordinates.  -}
rotationClasses :: [Coord] -> Map RotationClass [Coord]
rotationClasses asteroids = go others M.empty
  where 
    others = filter ((/=) origin) asteroids

    go (x:xs) m = go xs $ M.insertWith (++) (rotClass x) [x] m
    go [] m = m

{-  Generates a sequence of rotation class that in "clockwise" order. -}
rotationSequence :: Map RotationClass [Coord] -> [RotationClass]
rotationSequence m = 
  sortOn (\(x,y) -> atan2 (fromIntegral y) (fromIntegral x))
  $ fmap fst 
  $ M.toList m

{-  Takes out the next asteroid in the specified rotation class.  -}
takeOut :: Map RotationClass [Coord] -> RotationClass -> Maybe (Coord, Map RotationClass [Coord])
takeOut m rc = go $ sortOn abs2 $ (M.!) (m) $ rc
  where 
    go [] = Nothing -- All asteroids in this class have been destroyed
    go (x:xs) = Just $ (x, M.update (\_ -> Just xs) rc m)

type State = ([RotationClass], Map RotationClass [Coord] )

{-  Used in unfolr to generate the sequence of destroyed asteroids. 
    Models the destruction of the asteroid at the current laser 
    angle and moving on to the next.  -}
generator :: State -> Maybe (Maybe Coord, State)
generator ((rcX:rcXs), m) = 
  case isEmptyBelt of
    True -> Nothing
    False -> 
      case (takeOut m rcX) of
        Just (x, m') -> Just (Just $ x, (rcXs, m'))
        Nothing -> Just (Nothing, (rcXs, m))
  where
    isEmptyBelt = 
      L.all ((==) [])    
      $ fmap snd
      $ M.toList m

partOne :: [Coord] -> Int
partOne asteroids = 
  numberOfAsteroidsInSight asteroids
  $ bestLocation asteroids

partTwo :: [Coord] -> Int
partTwo asteroids = 100*x + y
  where 
    bestLoc = bestLocation asteroids
    rebasedAsteroids = fmap (flip (-#) bestLoc) asteroids

    equivClasses = rotationClasses rebasedAsteroids
    equivSeq = rotationSequence equivClasses

    infiniteSeq = dropWhile ((/=) (0,-1)) $ cycle equivSeq
    initialState = (infiniteSeq, equivClasses)

    destroySequence =
      fmap ((+#) bestLoc) 
      $ catMaybes 
      $ unfoldr generator initialState

    (x,y) = head $ drop 199 $ destroySequence

main :: IO ()
main = do
    parsedInput <- readDayLines 10
    let asteroids = pAsteroids $ parsedInput
    putStrLn $ "Part One: "  ++ (show $ partOne asteroids)
    putStrLn $ "Part Two: "  ++ (show $ partTwo asteroids)