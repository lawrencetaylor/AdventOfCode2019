module Day08 where

import Advent.Input
import Data.List
import Data.List.Split

newtype Row = Row [Char]
newtype Layer = Layer [Row]

{-  Logic for diplaying a Layer.  -}
instance Show Layer where
  show (Layer rows) = 
    intercalate "\n" $ fmap (\(Row str) -> display <$> str) rows
    where 
      display '0' = ' '
      display '1' = 'X'

{-  Rows can be combined using the "Top Visible Pixel" logic. -}
instance Semigroup Row where
  (Row pixels1) <> (Row pixels2) = Row $ zipWith getCh pixels1 pixels2
    where 
      getCh:: Char -> Char -> Char
      getCh '2' x = x
      getCh x _ = x

{-  Layers can be combined using the operation on Rows. -}
instance Semigroup Layer where
  (Layer rows1) <> (Layer rows2) = Layer $ zipWith (<>) rows1 rows2

{-  For the row with the minimum number of 0's, find the product 
    of the number of 1's and 2's. -}
partOne :: String -> Int
partOne input = (countOf '1' layer) * (countOf '2' layer)
  where    
    layer = 
      head
      $ sortOn (countOf '0')
      $ chunksOf (25 * 6) input
    countOf c = length . filter ((==) c)

{-  Determine the message using the "Top Visible Pixel" logic when
    one layer is put on top of another. -}
partTwo :: String -> Layer
partTwo input = foldr1 (<>) $ layers (25, 6) input
  where
    layers :: (Int, Int) -> String -> [Layer]
    layers (width, height) str = 
      Layer <$> (chunksOf height $ (Row <$> chunksOf width str))

main :: IO ()
main = do
    parsedInput <- readDay 8
    putStrLn $ "Part One: "  ++ (show $ partOne parsedInput)
    putStrLn $ "Part Two: \n"  ++ (show $ partTwo parsedInput)