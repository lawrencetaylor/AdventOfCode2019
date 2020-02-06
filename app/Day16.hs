module Day16 where

import           Advent.Input
import           Data.Char                     as C
import           Data.Either
import qualified Data.Map                      as M
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec (Parser)

-- Parsing
pSignal :: Parser [Int]
pSignal = do
  digits <- P.many P.digit
  return $ fmap C.digitToInt digits

-- Solution
base :: [Int]
base = [0, 1, 0, -1]

pattern :: Int -> [Int]
pattern i =
  drop 1 (cycle $ concat $ fmap (\b -> replicate i b) base)

sumProduct :: [Int] -> Int -> Int
sumProduct t i =
  onesDigit $
  sum $
  fmap (\(a, b) -> a * b) $
  filter (\(a, b) -> a * b /= 0) $ (pattern i) `zip` t

onesDigit :: Int -> Int
onesDigit n
  | n >= 0 = n `rem` 10
  | otherwise = onesDigit (-n)

phase :: [Int] -> [Int]
phase t = fmap (sumProduct t) [1 .. tLength]
  where
    tLength = length t

phases :: [Int] -> [[Int]]
phases t = iterate phase t

partOne t = concat $ fmap show $ take 8 $ (phases t) !! 100

offset :: [Int] -> Int
offset t =
  fromRight 0 $ parse pInt $ concat $ fmap show $ take 7 t

{-

If you do the math:

| a 	| b 	|  c 	| d 	| e 	|  f 	|  g 	| h 	|   FTT   	|
|:-:	|:-:	|:--:	|:-:	|:-:	|:--:	|:--:	|:-:	|:-------:	|
| a 	|   	| -c 	|   	| e 	|    	| -g 	|   	| a-c+e-g 	|
|   	| b 	| c  	|   	|   	| -f 	| -g 	|   	| b+c-f-g 	|
|   	|   	| c  	| d 	| e 	|    	|    	|   	| c+d+e   	|
|   	|   	|    	| d 	| e 	| f  	| g  	|   	| d+e+f+g 	|

|   	|   	|    	|   	| e 	| f  	| g  	| h 	| e+f+g+h 	|
|   	|   	|    	|   	|   	| f  	| g  	| h 	| f+g+h   	|
|   	|   	|    	|   	|   	|    	| g  	| h 	| g+h     	|
|   	|   	|    	|   	|   	|    	|    	| h 	| h       	|

For an n digit number x, given an offset of o > n/2, FTT[o] = Σ x[i] : i >= o

-}

next :: [Int] -> [Int]
next digits = (flip mod) 10 <$> scanr (+) 0 digits

partTwo :: [Int] -> String
partTwo digits = concat $ fmap show output
    where off = offset digits 
          allDigits = concat $ replicate 10000 digits
          afterOffset = drop off allDigits
          signalSequence = iterate next afterOffset
          output = take 8 $ signalSequence !! 100 

main :: IO ()
main = do
  Right parsedInput <- fmap (parse pSignal) $ readDay 16
  putStrLn $ "Part One: " ++ (show $ partOne parsedInput)
  putStrLn $ "Part Two: " ++ (show $ partTwo parsedInput)