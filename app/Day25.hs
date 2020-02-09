module Day25 where 
  
import Advent.Input
import Advent.Intcode
import Data.Char
import qualified Data.List as L

fromAscii :: String -> [Int]
fromAscii asciiInput = (fmap ord asciiInput) ++ [10]

execute :: Exe -> String -> IO Exe
execute exe input = do
  let Waiting exe' o = exe (fromAscii input)
  let output = fmap chr o
  putStrLn $ output
  return exe'

loop :: [String] -> Exe -> IO ()
loop commands exe = do
  let route = unlines $ reverse commands
  input <- getLine
  if(input == "q")
    then return ()
  else 
    (execute exe input) >>= loop (input:commands)

instructions = unlines 
  (
  [ "west"
  , "take cake"
  , "west"  
  , "take pointer"
  , "south"
  , "take monolith"
  , "north"
  , "west"
  , "south"
  , "take tambourine"
  , "east"
  -- , "take photons"
  , "south"
  -- , "take molten lava"
  , "north"
  , "east"
  , "east"
  , "take mug"
  , "west"
  , "west"
  , "west"
  , "north"
  , "east", "east", "east"
  , "south", "take coin", "east" , "take mouse", "south", "south"
  , "take hypercube"
  , "north", "north", "west", "south", "west" -- take infinite loop
  , "north" --, "take giant electromagnet"
  , "north"
  ] ++ (tryAllCombinations items)
  )-- 

items = [   "pointer"
          , "hypercube"
          , "cake"
          , "tambourine"
          , "monolith"
          , "mouse"
          , "coin"
          , "mug"
          ]

try items = 
  concat $
  [ dropCommands 
  , [ "north\n"]
  , pickCommands ]
  where
    dropCommands = fmap (\i -> "drop " ++ i ++ "\n") items
    pickCommands = fmap (\i -> "take " ++ i ++ "\n") items

tryAllCombinations items = 
  concat $
  fmap try $
  powerset items

powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

main :: IO ()
main = do
  Right program <- fmap (parse pProgram) $ readDay 25
  let Halt o = run program $ fmap ord $ instructions
  putStrLn $ (fmap chr o)

  -- Manual Navigation
  -- let Waiting exe o = run program []
  -- putStrLn $ (fmap chr o)
  -- loop [] exe


  