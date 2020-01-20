module Day20 where

import           Advent.Coordinates
import           Advent.Graph
import           Advent.Input
import           Algorithm.Search   (dijkstra)
import qualified Data.Bimap         as B
import qualified Data.List          as L
import qualified Data.Map           as M
import qualified Data.Maybe         as M
import           Safe





data Stop = Stop {
    position :: Point
  , portal   :: String
  , depth    :: Int
  } deriving (Eq, Ord)

instance Show Stop where
  show stop = show (portal stop, depth stop)





type Maze = M.Map Point Char

readMaze :: String -> Maze
readMaze str = go (0, 0) str M.empty
  where
    go _ [] m = m
    go (x, y) ('\n':cs) m = go (0, y + 1) cs m
    go (x, y) (c:cs) m = go (x + 1, y) cs newM
      where
        newM =
          case c of
            ' ' -> m
            _   -> M.insert (Point (x, y)) c m

mazePortals :: Maze -> Portals
mazePortals maze = Portals innerPath outerPath
  where
    walls = M.filter isWall maze

    Just (topLeft, '#') =
      fmap fst $ M.minViewWithKey walls

    Just (bottomRight, '#') =
      fmap fst $ M.maxViewWithKey walls

    readLabel d p = fmap ((M.!) maze) $ drop 1 $ scanl move p [d,d]

    labelOuter p
      | yCoord p == yCoord topLeft = reverse $ readLabel N p
      | xCoord p == xCoord bottomRight = readLabel E p
      | yCoord p == yCoord bottomRight = readLabel S p
      | xCoord p == yCoord topLeft = reverse $ readLabel W p

    labelInner p
      | yCoord p == yCoord topLeftInner = readLabel S p
      | xCoord p == xCoord bottomRightInner = reverse $ readLabel W p
      | yCoord p == yCoord bottomRightInner = reverse $ readLabel N p
      | xCoord p == yCoord topLeftInner = readLabel E p

    Point (outerLengthX, outerLengthY) = bottomRight <> (-|) topLeft
    outerPath = portals maze outerLengthX outerLengthY labelOuter topLeft

    topLeftInner =
      ((<>) (Point (-1, -1))) $
      head $
      filter (M.isNothing . (M.!?) maze) $
      iterate se $ topLeft

    bottomRightInner =
      ((<>) (Point (1, 1))) $
      head $
      filter (M.isNothing . (M.!?) maze) $
      iterate nw $ bottomRight

    Point (innerLengthX, innerLengthY) = bottomRightInner <> (-|) topLeftInner
    innerPath = portals maze innerLengthX innerLengthY  labelInner topLeftInner

    se p = foldl move p [S, E]
    nw p = foldl move p [N, W]

    doughnutWidth = xCoord (topLeftInner <> (-|) topLeft)

    isWall '#' = True
    isWall _   = False

portals :: Maze -> Int -> Int -> (Point -> String) -> Point -> B.Bimap String Point
portals maze xLength yLength label p =
  B.fromList $
  fmap (\p -> (label p, p)) $
  filter ((==) '.' . (M.!) maze) $
  drop 1 $
  scanl move p $
  concat
  [ take xLength $ repeat E
  , take yLength $ repeat S
  , take xLength $ repeat W
  , take yLength $ repeat N ]

data Portals = Portals {
    inner :: B.Bimap String Point
  , outer :: B.Bimap String Point
  } deriving (Show)

allPortalPoints :: Portals -> [Point]
allPortalPoints portals =
  L.nub $
  concat $
  [ B.keysR (inner portals)
  , B.keysR (outer portals) ]

maybePortal :: Portals -> Point -> Maybe String
maybePortal portals p =
  case
    M.catMaybes $
    [ (inner portals) !?> p
    , (outer portals) !?> p] of
  []  -> Nothing
  [a] -> Just a

start :: Portals -> Point
start portals =
  head $
  M.catMaybes $
  [ (inner portals) !? "AA"
  , (outer portals) !? "AA" ]

maybeTeleport :: Portals -> Point -> Maybe (Point, String)
maybeTeleport portals p =
  case ((inner portals) !?> p, (outer portals) !?> p) of
    (Nothing, Nothing) -> Nothing
    (Just portal, Nothing) -> fmap (\p -> (p, portal)) $ (outer portals) !? portal
    (Nothing, Just portal) -> fmap (\p -> (p, portal)) $ (inner portals) !? portal
    _ -> error "Portal found in inner and outer"

(!?) :: (Ord a, Ord b) => B.Bimap a b -> a -> Maybe b
(!?) biMap key =
  case key `B.member` biMap of
  True  -> Just $ biMap B.! key
  False -> Nothing

(!?>) :: (Ord a, Ord b) => B.Bimap a b -> b -> Maybe a
(!?>) biMap key =
  case key `B.memberR` biMap of
  True  -> Just $ biMap B.!> key
  False -> Nothing

portalDistances :: Maze -> Portals -> M.Map Point [(String, Point, Int)]
portalDistances maze portals = M.fromList steps
  where
    steps =
      fmap (\p -> (p, search maze portals p)) $
      allPortalPoints portals

    destinations p =
      let walking = search maze portals p
      in
        case maybeTeleport portals p of
          Just (p', s') -> walking ++ [(s',p',1)]
          Nothing       -> walking

search :: Maze -> Portals -> Point -> [(String, Point, Int)]
search maze portals point =
  M.catMaybes $
  fmap (\(p,n) -> fmap (\s -> (s,p,n)) $ maybePortal portals p) $
  filter ((/=) 0 . snd) $
  bfs nextMove fst (point, 0)
  where
    nextMove (p, n) =
      fmap (\p' -> (p', n+1)) $
      filter (\p -> (maze M.!? p) == (Just '.') ) $
      neighbours p


nextPortals :: M.Map Point [(String, Point, Int)] -> Portals -> (Stop -> Stop) -> Stop -> [Stop]
nextPortals distances portals teleport (Stop p s d) =
  filter takeOut $
  filter (\s -> (depth s) >= 0) $
  fmap (\(s', p', _) -> teleport $ Stop p' s' d) $
  distances M.! p
  where
    takeOut (Stop p s d)
      | d /= 0 = (s /= "AA") && (s /= "ZZ")
      | otherwise = True

teleportCost :: M.Map Point [(String, Point, Int)] -> Stop -> Stop -> Int
teleportCost distances (Stop p1 s1 _) (Stop p2 s2 _) =
  (\(_,_,n) -> n+1) $
  head $
  filter (\(s,p,n) -> s == s2) $
  distances M.! p1

type DepthChange = Int -> Int

maybeTeleportInner :: Portals -> Point -> Maybe Point
maybeTeleportInner portals p =
  (inner portals !?> p)
  >>= (\portal -> (outer portals) !? portal)

maybeTeleportOuter :: Portals -> Point -> Maybe Point
maybeTeleportOuter portals p =
  (outer portals !?> p)
  >>= (\portal -> (inner portals) !? portal)

run :: (DepthChange, DepthChange) -> Maze -> Int
run (onInner, onOuter) maze  = length - 1
  where
    portals = mazePortals maze
    distances = portalDistances maze portals
    cost = teleportCost distances
    initial = Stop (start portals) "AA" 0
    next = nextPortals distances portals teleport
    isFinished (Stop _ portal depth) = (portal == "ZZ") && (depth == 0)

    teleport (Stop p' s' d') =
      case (maybeTeleportInner portals p', maybeTeleportOuter portals p') of
      (Just p, Nothing)  -> Stop p s' (onInner d')
      (Nothing, Just p)  -> Stop p s' (onOuter d')
      (Nothing, Nothing) -> Stop p' s' d'

    Just length = fst <$> dijkstra next cost isFinished initial

partOne = run (id,id)

partTwo = run (\d -> d + 1, \d -> d - 1) 

main :: IO ()
main = do
  parsedInput <- readDay 20
  let maze = readMaze parsedInput
  putStrLn $ "Part One: " ++ (show $ partOne maze)
  putStrLn $ "Part Two: " ++ (show $ partTwo maze)






