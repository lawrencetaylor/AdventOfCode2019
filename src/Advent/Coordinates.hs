module Advent.Coordinates where

newtype Point =
  Point (Int, Int)
  deriving (Eq, Ord)

instance Show Point where
  show (Point p) = show p

data Direction
  = N | E | S | W
  deriving (Show, Eq)

instance Enum Direction where
  toEnum i =
    case i `mod` 4 of
      0 -> N
      1 -> E
      2 -> S
      3 -> W

  fromEnum N = 0
  fromEnum E = 1
  fromEnum S = 2
  fromEnum W = 3

move :: Point -> Direction -> Point
move (Point (x,y)) N = Point (x, y-1)
move (Point (x,y)) E = Point (x+1, y)
move (Point (x,y)) S = Point (x, y+1)
move (Point (x,y)) W = Point (x-1, y)

neighbours :: Point -> [Point]
neighbours p = take 4 $ fmap (move p) [north..]
  where north = N
