module Day18 where

import Advent.Coordinates
import Advent.Graph
import Advent.Input
import Algorithm.Search (dijkstra)
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as M
import qualified Data.Set as S

type Maze = M.Map Point Char

data Node
  = Initial Int
  | Key Char
  deriving (Show, Eq, Ord)

readMaze :: String -> Maze
readMaze str = go (0, 0) str M.empty
  where
    go _ [] m = m
    go (x, y) ('@':cs) m =
      go (x + 1, y) cs (M.insert (Point (x, y)) '@' m)
    go (x, y) ('\n':cs) m = go (0, y + 1) cs m
    go (x, y) (c:cs) m = go (x + 1, y) cs newM
      where
        value '.' = Nothing
        value c = Just c
        newM =
          case value c of
            Nothing -> m
            Just v -> M.insert (Point (x, y)) v m

modify :: Maze -> Maze
modify maze =
  M.insert (foldl move p [S, W]) '@' $
  M.insert (foldl move p [S, E]) '@' $
  M.insert (foldl move p [N, W]) '@' $
  M.insert (foldl move p [N, E]) '@' $
  M.insert (move p W) '#' $
  M.insert (move p S) '#' $
  M.insert (move p E) '#' $
  M.insert (move p N) '#' $ M.insert p '#' maze
  where
    nodes = reduceToNodes maze
    p = nodes M.! (Initial 0)

{-  First we reduce the maze to a graph of nodes we care about: 
    the initial starting point and keys.  
    
    The edges of the graph will contain:
      * The distance -}

reduceToNodes :: Maze -> M.Map Node Point
reduceToNodes maze = foldr grow M.empty $ M.toList maze
  where
    grow (p, c) m
      | C.isLower c = M.insert (Key (C.toUpper c)) p m
      | C.isUpper c = m
    grow (p, '@') m = M.insert (Initial n) p m
      where
        n = length $ filter isInitial $ M.keys m
    grow (p, '#') m = m

positionOfNodes :: Maze -> M.Map Point Node
positionOfNodes maze =
  M.fromList $
  fmap (\(a, b) -> (b, a)) $ M.toList $ reduceToNodes maze

partOne :: Maze -> Int
partOne = M.fromJust . fmap fst . shortest

partTwo :: Maze -> Int
partTwo = M.fromJust . fmap fst . shortest . modify

main :: IO ()
main = do
  parsedInput <- readDay 18
  let m = readMaze parsedInput
  putStrLn "Go get a tea, this takes a while..."
  putStrLn $ "Part One: " ++ (show $ partOne m)
  putStrLn $ "Part Two: " ++ (show $ partTwo m)

isKey (Key c) = True
isKey _ = False

isInitial (Initial _) = True
isInitial _ = False

initial :: Maze -> Point
initial maze =
  fst $ head $ M.toList $ M.filter ((==) '@') maze

{-  Given a point, calculates all the possible destinations (Keys), 
    along with their distance from the current point, and the set of keys
    required to get there (to go through the doors in the way). -}
search ::
     ((Point, Int, S.Set Char) -> [(Point, Int, S.Set Char)])
  -> M.Map Point Node
  -> Point
  -> [Destination]
search nextSteps nodes start =
  M.catMaybes $
  fmap maybeDestination $
  bfs nextBfs projBfs (start, 0, S.empty)
  where
    nextBfs = nextSteps
    projBfs (p, _, _) = p

    maybeDestination (p, n, reqDoors) =
      fmap (\(Key node) -> (Destination node n reqDoors)) $
      (nodes M.!? p) >>= (\c -> if (isKey c) then Just c else Nothing) --  Maybe Node

{-  From a given (Position, Distance, RequiredKeys) finds all
    the next possible values by stepping from this point. -}
nextStep ::
     Maze
  -> (Point, Int, S.Set Char)
  -> [(Point, Int, S.Set Char)]
nextStep maze (current, n, requiredKeys) =
  fmap (\p -> (p, n + 1, addRequiredKey p)) $
  filter isNotWall $ neighbours current
  where
    isNotWall = (/=) (Just '#') . (M.!?) maze

    {-  If we have reached a door, add to the required
        key collection. -}
    addRequiredKey p =
      M.fromMaybe requiredKeys $
      fmap
        (\c ->
           if C.isUpper c
             then S.insert c requiredKeys
             else requiredKeys) $
      maze M.!? p

data Destination =
  Destination
    { key :: Char
    , distance :: Int
    , requiredKeys :: S.Set Char
    }
  deriving (Show)

destinations :: Maze -> M.Map Node [Destination]
destinations maze =
  M.fromList $
  fmap (\k -> (k, possible k)) $
  M.keys $ nodePositions
  where
    nodePositions = reduceToNodes maze
    positionNodes = positionOfNodes maze
    next = nextStep maze
    possible k =
      filter ((/=) 0 . distance) $ -- Ignore Start
      search next positionNodes $ nodePositions M.! k

data Journey =
  Journey
    { current :: [Node]
    , collectedKeys :: S.Set Char
    }
  deriving (Show, Eq, Ord)

nextKeys :: M.Map Node [Destination] -> Journey -> [Journey]
nextKeys nodes (Journey ns collected) =
  concat $ fmap singleNodeJourneys ns
  where
    singleNodeJourneys n =
      fmap (\(n', c') -> Journey (L.insert n' $ L.delete n ns) c') $ -- Modify current
      fmap (\d -> let n = key d in (Key n, S.insert n collected)) $ -- Add to collected keys
      filter (\d -> not ((key d) `S.member` collected)) $ -- Don't visit collected key
      filter (\d -> (requiredKeys d) `S.isSubsetOf` collected) $ -- Need all keys for doors
      nodes M.! n

initials :: Maze -> Journey
initials maze = Journey initialPositions S.empty
  where
    initialPositions =
      fmap snd $
      M.toList $ M.filter (isInitial) $ 
      positionOfNodes maze

shortest maze = dijkstra next cost isFinished initialJ
  where
    d = destinations maze
    next = nextKeys d
    initialJ = initials maze
    cost j1 j2 =
      distance $
      head $
      filter (\dest -> (key dest) == n2) $ (d M.! n1)
      where
        ns1 = S.fromList $ current j1
        ns2 = S.fromList $ current j2
        [n1] = S.toList $ ns1 `S.difference` ns2
        [(Key n2)] = S.toList $ ns2 `S.difference` ns1
    allKeys =
      S.fromList $
      fmap (\(Key c) -> c) $
      filter (not . isInitial) $ M.keys d
    isFinished j = ((collectedKeys j) == allKeys)
