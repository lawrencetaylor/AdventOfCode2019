module Advent.Coordinates where

newtype Point =
  Point (Int, Int)
  deriving (Eq, Ord)

instance Show Point where
  show (Point p) = show p

instance Semigroup Point where
  (Point (x1, y1)) <> (Point (x2, y2)) =
    Point (x1 + x2, y1 + y2)

instance Monoid Point where
  mempty = origin

data Direction
  = N
  | E
  | S
  | W
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

origin = Point (0, 0)

move :: Point -> Direction -> Point
move (Point (x, y)) N = Point (x, y - 1)
move (Point (x, y)) E = Point (x + 1, y)
move (Point (x, y)) S = Point (x, y + 1)
move (Point (x, y)) W = Point (x - 1, y)

neighbours :: Point -> [Point]
neighbours p = take 4 $ fmap (move p) [north ..]
  where
    north = N

xCoord :: Point -> Int
xCoord (Point (x, _)) = x

yCoord :: Point -> Int
yCoord (Point (_, y)) = y

(-|) :: Point -> Point
(-|) (Point(x,y)) = Point(-x,-y)
