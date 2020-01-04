{-# LANGUAGE NumericUnderscores #-}

module Day14 where

import Text.ParserCombinators.Parsec(Parser)
import qualified Text.Parsec as P(char, letter, many1, sepBy, string)
import Advent.Input
import Data.Map as M
import Data.Maybe
import Data.List as L
import Data.Tree

type Amount = Int
type Chemical = String
type Ingredient = (Amount, Chemical)
type Reaction = (Chemical, Amount, [Ingredient])
type Reactions = Map Chemical (Amount, [Ingredient])
type Requirements = Map String Amount

-- Parsing

pIngredient :: Parser Ingredient
pIngredient = do
  count <- pInt
  P.char ' '
  word <- P.many1 P.letter
  return (fromIntegral count, word)

pReaction :: Parser Reaction
pReaction = do
  let seperator = P.string ", "
  ingredients <- P.sepBy pIngredient seperator
  P.string " => "
  (rCount, rWord) <- pIngredient
  return (rWord, rCount, ingredients)

pReactions :: Parser Reactions
pReactions = do
  let seperator = P.char '\n'
  reactions <- P.sepBy pReaction seperator
  return $ M.fromList $ fmap (\(p,a,i) -> (p, (a, i))) reactions

-- Solution

{-  Creates a tree of dependencies of the distinct
    chemical elements.  -}
dependencies :: Reactions -> Tree Chemical
dependencies reactions = unfoldTree go "FUEL"
  where
    go chemical = (chemical, fmap snd $ snd $ fromMaybe (0, []) $ reactions M.!? chemical)

{-  Sorts the chemical dependency tree in to a list of elements
    such that when the list is traversed, when we reach 
    a chemical X those chemicals that depend on X have already 
    been processed.  -}
topologicalSort :: (Eq a) => Tree a -> [a]
topologicalSort t = go (postOrder t) []
  where
    go [] acc = acc
    go (x:xs) acc
      | x `L.elem` acc = go xs acc
      | otherwise = go xs (x:acc)

    postOrder :: Tree a -> [a]
    postOrder (Node node forest) =
      (concat $ fmap postOrder forest) ++ [node]

{-  Breaks down the specified chemical, returning an updated
    set of requirements based on the reactions that occurred. -}
breakDown :: Reactions -> Requirements -> Chemical -> Requirements
breakDown reactions req "ORE" = req
breakDown reactions req chem = newRequirements
  where
    requiredAmt = req M.! chem
    (producedAmt, ingredients) = reactions M.! chem

    reactionCount =
      case (requiredAmt `rem` producedAmt) of
        0 -> requiredAmt `div` producedAmt
        _ -> 1 + (requiredAmt `div` producedAmt)

    newIngredients = fmap (\(a,c) -> (a * reactionCount, c)) ingredients

    newRequirements =
      L.foldr
        (\(iAmount, iChem) m -> M.insertWith (+) iChem iAmount m)
        (M.delete chem req)
        newIngredients

requiredOre :: Reactions -> Int -> Int
requiredOre reactions fuelCount =
  snd $
  head $
  M.toList $
  L.foldl (breakDown reactions) required topOrder
  where
    required = M.fromList $ [("FUEL", fuelCount)]
    topOrder = topologicalSort . dependencies $ reactions

binarySearch :: Int -> Int -> Int -> (Int -> Int) -> Int
binarySearch lower upper threshold f
  | (upper == lower) = lower
  | (upper == lower + 1) = lower
  | ((f middle) > threshold) = binarySearch lower middle threshold f
  | otherwise = binarySearch middle upper threshold f
  where 
    middle = (upper + lower) `div` 2

partOne :: Reactions -> Int
partOne reactions = requiredOre reactions 1

partTwo :: Reactions -> Int
partTwo reactions = binarySearch 1 initialUpperBound threshold oreCount
  where
    oreCount = requiredOre reactions
    initialUpperBound =  10_000_000
    threshold = 1_000_000_000_000

main :: IO ()
main = do
  Right parsedInput <- fmap (parse pReactions) $ readDay 14
  putStrLn $ "Part One: " ++ (show $ partOne parsedInput)
  putStrLn $ "Part Two: " ++ (show $ partTwo parsedInput)
