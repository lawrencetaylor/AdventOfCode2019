module Day12 where

import Data.List as L
import Data.Set as S

type Coordinate = (Int,Int,Int)
type Position = Coordinate
type Velocity = Coordinate
type Acceleration = Coordinate
type MoonState = (Position, Velocity)

initialMoons :: [MoonState]
initialMoons = 
  fmap (\x -> (x, zero))
  [ (-3, 15,-11)
  , ( 3, 13,-19)
  , (-13, 18, -2)
  , (6, 0,-1)
  ]

zero :: Coordinate
zero = (0,0,0)

(+#) :: Coordinate -> Coordinate -> Coordinate
(+#) (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)

gravity :: Position -> Position -> Acceleration
gravity  (x1,y1,z1) (x2,y2,z2) = 
  (apply x1 x2, apply y1 y2, apply z1 z2)
  where
    apply p1 p2 = 
      if p1 < p2 then 1
      else if p1 > p2 then -1
      else 0

acceleration :: [Position] -> Position -> Acceleration
acceleration moons p = 
  L.foldr (+#) zero
  $ fmap (gravity p) moons

tick :: [MoonState] -> [MoonState]
tick moons = 
  fmap (\(p, v, a) -> 
    let 
      v' = v +# a
      p' = p +# v'
    in (p', v'))
  $ fmap (\(p,v) -> (p,v,accel p))
  $ moons
  where
    accel = acceleration ( fmap fst moons)

energy :: MoonState -> Int
energy (p,v) = (go p)*(go v)
  where go (x,y,z) = sum [abs x, abs y, abs z]

timeToFirstRepeat :: (MoonState -> (Int,Int)) -> [[MoonState]] -> Int
timeToFirstRepeat projection moons = go moons S.empty 0
  where
    proj = fmap projection

    go :: [[MoonState]] -> Set [(Int,Int)] -> Int -> Int
    go (x:xs) seen acc = 
      case (S.member (proj x) seen) of
      True -> acc
      False -> go xs (S.insert (proj x) seen) (acc + 1)

partOne :: [MoonState] -> Int
partOne moonSystem = sum $ fmap energy $ ( iterate tick moonSystem) !! 1000

partTwo :: [MoonState] -> Int
partTwo moonSystem = 
  leastCommonMultiple [
    timeToFirstRepeat (\((x,_,_), (a,_,_)) -> (x,a)) seq
  , timeToFirstRepeat (\((_,y,_), (_,b,_)) -> (y,b)) seq
  , timeToFirstRepeat (\((_,_,z), (_,_,c)) -> (z,c)) seq ]
  where 
    leastCommonMultiple = L.foldl lcm 1
    seq = iterate tick moonSystem

main :: IO ()
main = do
  putStrLn $ "Part One: " ++ (show $ partOne initialMoons)
  putStrLn $ "Part Two: " ++ (show $ partTwo initialMoons)