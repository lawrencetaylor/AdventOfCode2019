module Day22 where

import Advent.Input
import qualified Data.List as L
import qualified Data.Map as M
import qualified Text.Parsec as P ((<|>), string, try)
import Text.ParserCombinators.Parsec (Parser)

data Instruction
  = NewStack
  | Cut Integer
  | Increment Integer
  deriving (Show)

{- (a, b) represents an affine map of the form
    x -> ax + b   -}
type LinearTransformation = (Integer, Integer)

-- Parsing
pNewStack :: Parser Instruction
pNewStack = do
  P.try $ P.string "deal into new stack"
  return NewStack

pCut :: Parser Instruction
pCut = do
  P.try $ P.string "cut "
  n <- fmap toInteger pInt
  return $ Cut n

pIncrement :: Parser Instruction
pIncrement = do
  P.try $ P.string "deal with increment "
  n <- fmap toInteger pInt
  return $ Increment n

pInstruction :: Parser Instruction
pInstruction = pNewStack P.<|> pCut P.<|> pIncrement

{-
Composing LinearTransformations:
f : x -> ax + b
g : x -> cx + d
g \circ f :
    x -> c (ax + b) + d
      -> acx + (bc + d)
-}
compose ::
     Integer
  -> LinearTransformation
  -> LinearTransformation
  -> LinearTransformation
compose size (c, d) (a, b) =
  ( (a * c) `mod` size
  , ((b * c) `mod` size) + (d `mod` size))

{-
NewStack:
  Position of 0 = -1
  x -> -x - 1
  (-1, -1) \circ (a,b) -> -(-a, -b-1)

Cut n:
  Position of 0 = -n
  Cut 4: [0,1,2,3,4,5] -> [4,5,0,1,2,3]
  x -> x - n
  (1, -1) \circ (a,b) -> (a, -b - n)

Increment n:
  Position of 0 = 0
  Position of 1 = n
  Increment 3 : [0,1,2,3,4,5,6] -> [0,5,3,1,6,4,2]
  x -> n*x
  (n, 0) \circ (a,b) -> (na, b)
-}
apply ::
     Integer
  -> Instruction
  -> LinearTransformation
  -> LinearTransformation
apply size NewStack (a, b) = compose size (-1, -1) (a, b)
apply size (Cut n) (a, b) = compose size (1, -n) (a, b)
apply size (Increment n) (a, b) = compose size (n, 0) (a, b)

{-  Given two coprime integers (a,b) returns
    a pair of integers (x, y) such that
    ax + by = 1 -}
bezout :: Integer -> Integer -> (Integer, Integer)
bezout a b = go a b 1 0 0 1
  where
    go a' 0 x y _ _ = (x, y)
    go a' b' cX cY x y = go b' r x y x' y'
      where
        (q, r) = quotRem a' b'
        x' = cX - q * x
        y' = cY - q * y

{-  Given an integer a coprime to n, calculates
    the inverse of a modulo n.  -}
inverse :: Integer -> Integer -> Integer
inverse n = (flip mod) n . snd . bezout n

{-  Returns the linear transformation representing
    a single shuffle of the cards.  -}
singleShuffle ::
     [Instruction] -> Integer -> LinearTransformation
singleShuffle instructions size = (a, b)
  where
    (a, b) =
      (\f -> f (1, 0)) $
      foldr (flip (.)) id $ fmap (apply size) instructions

{-  Returns an array of 0/1's in the binary
    representation of m.  -}
toBinary m = go m []
  where
    go n acc
      | n == 0 = acc
      | r == 1 = go q (1 : acc)
      | r == 0 = go q (0 : acc)
      where
        (q, r) = quotRem n 2

{-  Efficient algorithm for computing powers in a group.  -}
pow :: (Show a) => (a -> a -> a) -> Integer -> a -> a
pow f power n =
  foldl1 f $
  fmap fst $
  filter ((==) 1 . snd) $
  zip powers $ reverse $ toBinary power
  where 
    powers = iterate (\a -> f a a) n

partOne :: [Instruction] -> Integer -> Integer -> Integer
partOne instructions size n = f n
  where
    f x = (a * x + b) `mod` size
    (a, b) = singleShuffle instructions size
    
partTwo :: [Instruction] -> Integer -> Integer -> Integer -> Integer
partTwo instructions size iterations n =
  ((n - d) * (inverse size c)) `mod` size
  where
    (a, b) = singleShuffle instructions size
    (c, d) = pow (compose size) iterations (a, b)
    -- position = c*value+ d
    -- value = (position - d) * (1/c)

main :: IO ()
main = do
  Right instructions <- traverse (parse pInstruction) <$> readDayLines 22
  putStrLn $ "Part One: " ++ (show $ partOne instructions 10007 2019)
  let cardCount = 119315717514047
  let iterations = 101741582076661
  putStrLn $ "Part Two: " ++ (show $ partTwo instructions cardCount iterations 2020)