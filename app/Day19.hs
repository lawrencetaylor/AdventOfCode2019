module Day19 where 
  
import Advent.Intcode
import Advent.Input
import qualified Data.List as L
import qualified Data.Set as S

isPulled program x y = o == 1
  where 
    Halt [o] = run program [x,y]

closestSquare program x y = 
  case (isP (x,y), isP (x+99,y-99)) of
  (False, _) -> go (x+1) y
  (True, False) -> go x (y+1)
  (True, True) -> (x, y-99)
  where 
    isP (x,y) = isPulled program x y
    go = closestSquare program

partOne :: State -> Int
partOne program = 
  length $ 
  [ (x,y) | x <- [0..49], y <- [0..49], isPulled program x y ]
    
partTwo :: State -> Int
partTwo program = 10000 * x + y
  where (x, y) = closestSquare program 0 100

main :: IO ()
main = do
  Right program <- fmap (parse pProgram) $ readDay 19
  putStrLn $ "Part One: " ++ (show $ partOne program)
  putStrLn $ "Part Two: " ++ (show $ partTwo program)