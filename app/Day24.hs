module Day24 where

import Advent.Coordinates
import Advent.Input
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as M
import qualified Data.Set as S

type Bugs = M.Map Point Char
type BugLevels = M.Map (Int, Point) Char

-- Display

draw :: Bugs -> String
draw bugs = foldl go [] pairs
  where
    pairs = L.sortOn fst $ M.assocs bugs
    go str (Point (n, 0), c) = str ++ ['\n', c]
    go str (Point (n, _), c) = str ++ [c]

drawLevel :: BugLevels -> Int -> String
drawLevel bugLevels level =
  draw $
  M.mapKeys (snd) $
  M.filterWithKey (\(l, _) _ -> l == level) bugLevels

drawLevels :: BugLevels -> String
drawLevels bugLevels =
  L.intercalate "\n\n" $
  [ "Level " ++ (show l) ++ ":\n" ++ (drawLevel bugLevels l)
  | l <- [minLevel .. maxLevel]
  ]
  where
    levels = fst . fst <$> M.assocs bugLevels
    maxLevel = maximum levels
    minLevel = minimum levels

pBugs :: [String] -> Bugs
pBugs s =
  M.fromList $
  [ (Point (y, x), c)
  | (y, line) <- rows
  , (x, c) <- zip [0 ..] line
  ]
  where
    rows = zip [0 ..] s

-- Parsing

pLevels :: [String] -> BugLevels
pLevels s =
  M.insert (0, Point (2, 2)) '?' $
  M.fromList $
  [ ((0, (Point (y, x))), c)
  | (y, line) <- rows
  , (x, c) <- zip [0 ..] line
  ]
  where
    rows = zip [0 ..] s

-- Solution

type Neighbours a = a -> [a]

type Expand a = M.Map a Char -> M.Map a Char

next ::
     (Ord a)
  => Neighbours a
  -> M.Map a Char
  -> a
  -> Char
  -> Char
next pointNeighbours bugs p c
  | (c == '#') && (adjacentBugs /= 1) = '.'
  | (c == '.') && (adjacentBugs == 1) = '#'
  | (c == '.') && (adjacentBugs == 2) = '#'
  | otherwise = c
  where
    hasBug p =
      M.fromMaybe False $ fmap ((==) '#') $ M.lookup p bugs
    adjacentBugs =
      length $ filter (hasBug) $ pointNeighbours p

tick ::
     Ord a
  => Expand a
  -> Neighbours a
  -> M.Map a Char
  -> M.Map a Char
tick expandPoints pointNeighbours bugs =
  M.mapWithKey (next pointNeighbours newBugs) newBugs
  where
    newBugs = expandPoints bugs

evolution ::
     Ord a
  => Expand a
  -> Neighbours a
  -> M.Map a Char
  -> [M.Map a Char]
evolution expandPoints pointNeighbours bugs =
  iterate (tick expandPoints pointNeighbours) bugs

firstDupe :: Bugs -> Bugs
firstDupe bugs = go S.empty (evolution id neighbours bugs)
  where
    go seen (x:xs) =
      case x `S.member` seen of
        True -> x
        False -> go (S.insert x seen) xs

biodiversity :: Bugs -> Int
biodiversity bugs =
  sum $
  fmap fst $
  filter ((==) '#' . snd . snd) $
  zip (iterate (* 2) 1) $ L.sortOn fst $ M.assocs bugs

{-  Neighbouring points after falling "into" a level
    after travelling in the given direction.  -}
fallIn :: M.Map Direction [Point]
fallIn =
  M.fromList $
  [ (N, (\i -> Point (i, 4)) <$> [0 .. 4])
  , (E, (\i -> Point (0, i)) <$> [0 .. 4])
  , (S, (\i -> Point (i, 0)) <$> [0 .. 4])
  , (W, (\i -> Point (4, i)) <$> [0 .. 4])
  ]

{-  Adjusts any neighbouring positions that fall in/out
    of the current levels in to those above/below.  -}
adjustLevel :: Direction -> Int -> Point -> [(Int, Point)]
adjustLevel d l (Point (2, 2)) =
  (\p -> (l + 1, p)) <$> (fallIn M.! d)
adjustLevel d l (Point (-1, _)) = [(l - 1, Point (1, 2))]
adjustLevel d l (Point (_, 5)) = [(l - 1, Point (2, 3))]
adjustLevel d l (Point (5, _)) = [(l - 1, Point (3, 2))]
adjustLevel d l (Point (_, -1)) = [(l - 1, Point (2, 1))]
adjustLevel _ l p = [(l, p)]

{-  Gets the points on the levels we need to check
    for deaths/infection. -}
points :: BugLevels -> [(Int, Point)]
points bugLevels =
  [ (l, Point (x, y))
  | l <- [(minLevel - 1) .. (maxLevel + 1)]
  , x <- [0 .. 4]
  , y <- [0 .. 4]
  ]
  where
    levels =
      fst . fst <$> M.assocs (M.filter ((==) '#') bugLevels)
    maxLevel = maximum levels
    minLevel = minimum levels

ex l = M.mapWithKey (insertQuestion) $ expand l (points l)
  where
    expand :: Ord k => M.Map k Char -> [k] -> M.Map k Char
    expand bugs newKeys =
      M.fromList $
      fmap (\k -> (k, M.fromMaybe '.' $ M.lookup k bugs)) $
      newKeys

    insertQuestion (_, Point (2, 2)) c = '?'
    insertQuestion _ c = c

input = ["#..#.", "..#..", "...##", "...#.", "#.###"]

partOne :: Bugs -> Int
partOne = biodiversity . firstDupe

partTwo :: BugLevels -> Int
partTwo bugLevels = bugCount $ (evolution ex recursiveNeighbours bugLevels) !! 200
  where 
    bugCount = length . M.filter ((==) '#')
    recursiveNeighbours (level, p) =
      concat $
      fmap (\d -> adjustLevel d level $ move p d) $ [N, E, S, W]

main :: IO ()
main = do
  putStrLn $ "Part One: " ++ (show $ partOne $ pBugs input)
  putStrLn $ "Part Two: " ++ (show $ partTwo $ pLevels input)