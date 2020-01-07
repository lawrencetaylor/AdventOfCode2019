module Day17 where

import Advent.Input
import Advent.Intcode
import Data.Char (chr, ord)
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M

{-  Returns a tuple containing:
    * Set of scaffold points
    * Location of the robot -}
toSet :: String -> (S.Set (Int,Int), (Int,Int))
toSet str = go (0,0) S.empty (0,0) str
  where
    go :: (Int,Int) -> S.Set (Int,Int) -> (Int,Int) -> String -> (S.Set (Int,Int), (Int,Int))
    go (x,y) s r ('.':xs) = go (x+1,y) s r xs
    go (x,y) s r ('#':xs) = go (x+1,y) (S.insert (x,y) s) r xs
    go (x,y) s r ('\n':xs) = go (0, y+1) s r xs
    go (x,y) s r ('^':xs) = go (x+1, y) (S.insert (x,y) s) (x,y) xs
    go (x,y) s r [] = (s,r)
 
draw :: State -> String
draw program = 
  unlines $
  [ fmap toChar $ fmap (\x -> (x,y)) [minX..maxX] | y <- [minY..maxY] ]
  where 
    minX = minimum $ S.map fst s
    minY = minimum $ S.map snd s
    maxX = maximum $ S.map fst s
    maxY = maximum $ S.map snd s

    toChar c = 
      if c == t then '^' 
      else if c `S.member` s then '#' 
      else '.'

    (s, t) = toSet $ fmap chr output
    Halt output = run program []

data Direction = N | E | S | W deriving (Show, Eq)
type Path = [(Direction, Int)]
type Position = (Int,Int)
data Turn = L | R deriving (Show, Eq)

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

step :: Direction -> Position -> Position
step N (x,y) = (x, y-1)
step E (x,y) = (x+1, y)
step S (x,y) = (x, y+1)
step W (x,y) = (x-1, y)

turnFromDirectionChange :: Direction -> Direction -> Turn
turnFromDirectionChange d1 d2
  | d2 == (succ) d1 = R
  | d2 == (pred) d1 = L

turn :: Direction -> Turn -> Direction
turn d R = succ d
turn d L = pred d

pathUnfold :: S.Set Position 
        -> (Position, Direction)  
        -> Maybe ((Turn, Int), (Position, Direction))
pathUnfold s (p, d) = 
  case possibleTurns of
  [t] -> 
    Just ((turnFromDirectionChange d newD, length moves), finalPosition)
    where 
      newD = turn d t
      moves = steps newD
      finalPosition = (last moves, newD)
  [] -> Nothing
  where 
    possibleTurns = 
      L.filter (\t -> (step (turn d t) p) `S.member` s) 
      [L,R]

    steps dir = 
      drop 1 $
      takeWhile (\p' -> p' `S.member` s) $
      iterate (step dir) p
    
{-  Returns a list of instructions that creates a path along the 
    scaffold that reaches every part of the scaffold once.  -}
path :: State -> [(Turn, Int)]
path program = L.unfoldr (pathUnfold s) (t, N)
  where 
    (s, t) = toSet $ fmap chr output
    Halt output = run program []

partOne :: State -> Int
partOne program = 
  sum $ 
  S.map (\(x,y) -> x * y) $ 
  S.filter (isIntersection s) s
  where 
    isIntersection s (x,y) = 
      all (\p -> p `S.member` s)
      [ (x+1,y)
      , (x, y+1)
      , (x-1, y)
      , (x, y-1) ]

    Halt output = run program []

    (s, r) = toSet $ fmap chr output

partTwo :: State -> Int
partTwo program = last $ output
  where
    -- First run program to get plan
    reg = register program
    newProg = program { register = M.insert 0 2 reg }
    pathProgram = path program

    -- Solved this by hand :(
    m = "A,B,A,B,C,C,B,C,B,A"
    a = "R,12,L,8,R,12"
    b = "R,8,R,6,R,6,R,8"
    c = "R,8,L,8,R,8,R,4,R,4"

    i = L.intercalate "\n" [m,a,b,c,"n"] ++ "\n"

    Halt output = run newProg $ fmap ord i

main :: IO ()
main = do
  Right parsedInput <- fmap (parse pProgram) $ readDay 17
  -- putStrLn $ (draw parsedInput)
  putStrLn $ "Part One: " ++ (show $ partOne parsedInput)
  putStrLn $ "Part Two: " ++ (show $ partTwo parsedInput)

{- 
............#############....................
............#...........#....................
............#.....#########..................
............#.....#.....#.#..................
............#.....#.....#.#..................
............#.....#.....#.#..................
............#.....#.....#.#..................
............#.....#.....#.#..................
^AAAAAAAAAAAA.....#######.#..................
..........................#..................
..........................#..................
..........................#..................
..........................#.#................
..........................#.#................
..........................#########..........
............................#.....#..........
............................#.....#..........
............................#.....#..........
............................#.....#..........
............................#.....#..........
..........................#######.#..........
..........................#.#...#.#..........
..........................#.#...#.#..........
..........................#.#...#.#..........
..........................#.#########........
..........................#.....#.#.#........
..........................#########.#........
................................#...#........
........................#########...#........
........................#...........#........
........................#...........#.#######
........................#...........#.#.....#
................#####...#...........#.#.....#
................#...#...#...........#.#.....#
................#...#...#.........#########.#
................#...#...#.........#.#.#...#.#
................#########.........#.#########
....................#.............#...#...#..
....................#.............#...#####..
....................#.............#..........
....................#########.....#..........
............................#.....#..........
..........................#########..........
..........................#.#................
........................#########............
........................#.#.#...#............
........................#.#.#...#............
........................#.#.#...#............
........................#####...#............
..........................#.....#............
..........................#######............
-}