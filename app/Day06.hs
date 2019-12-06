module Day06 where

import Data.Tuple
import Data.Maybe
import Text.ParserCombinators.Parsec(Parser)
import Text.Parsec(char, many, letter, digit, (<|>))
import Advent.Input
import Data.Tree
import Data.Map(Map, (!))
import Data.Map as M(lookup, fromList)
import Data.Set(Set, difference)
import qualified Data.Set as S(fromList, toList)
import Data.List(nub)
import Algorithm.Search(dijkstra)

type Planet = String
type Satellite = String
type HeavenlyBody = String
type Pair = (Planet, Satellite)

-- Parsing

pOrbit :: Parser (Planet, Satellite)
pOrbit = do
  planet <- many $ (letter <|> digit)
  char ')'
  moon <- many $(letter <|> digit)
  return (planet , moon)

-- Solution

{-  Creates a mapping from a Planet to it's satellites.  -}
satellites :: [(Planet,Satellite)] -> Map Planet [Satellite]
satellites pairs = 
  fromList 
  $ fmap (\k -> (k, fmap snd $ filter ((==) k . fst) pairs)) 
  $ nub . fmap fst $ pairs

{-  Finds the single planet that is not a satellite 
    to any other planet.  -}
root :: [(Planet,Satellite)] -> Planet
root pairs = root
  where
    keys = S.fromList . fmap fst $ pairs
    values = S.fromList. fmap snd $ pairs
    [root] = S.toList $ keys `difference` values

{-  Creates a tree of heavenly bodies, where the root of the 
    tree is the single planet that does not orbit any other 
    planet. -}
createTree :: [(Planet,Satellite)] -> Tree HeavenlyBody
createTree pairs = unfoldTree c $ root pairs
  where
    maps = satellites pairs
    c x = 
      case M.lookup x maps of
      Just y -> (x, y)
      Nothing -> (x, [])

{-  Finds the total number of direct and indirect orbits of all the 
    heavenly bodies.  This boils down to finding the sum of all
    the depths of the nodes in the tree.  -}
partOne :: [(String, String)] -> Int
partOne t = inner 0 $ createTree t
  where
    inner :: Int -> Tree a -> Int
    inner running (Node root []) = running
    inner running (Node root tree) =  running + sum (fmap (inner $ running + 1) tree)

{-  Find the shortest path between me ("YOU") and santa ("SAN"), 
    where we are allowed to hop between heavenly bodies where one
    orbits the other.  -}
partTwo :: [(String, String)] -> Int
partTwo pairs = length - 2
  where
    suns = satellites $ fmap swap pairs
    planets = satellites pairs

    possibleHops :: String -> [String]
    possibleHops s = 
      concat
      $ catMaybes
      $ [ (M.lookup s planets)
        , (M.lookup s suns) ]

    cost _ _ = 1

    Just (length, _) = dijkstra possibleHops cost ((==) "SAN") "YOU"

main :: IO ()
main = do
    parsedInput <- (fmap $ traverse id) $ (fmap $ fmap $ parse pOrbit) $ readDayLines 6
    let (Right orbits) = parsedInput
    putStrLn $ "Part One: "  ++ (show $ partOne orbits)
    putStrLn $ "Part Two: "  ++ (show $ partTwo orbits)