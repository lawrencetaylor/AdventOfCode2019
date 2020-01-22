module Day21 where 
  
import Advent.Input
import Advent.Intcode
import qualified Data.Set as S
import qualified Data.List as L
import Data.Char

toSet :: String -> (S.Set (Int,Int), Maybe String  )
toSet str = go (0,0) S.empty str
  where
    go :: (Int,Int) -> S.Set (Int,Int) -> String -> (S.Set (Int,Int), Maybe String)
    go (x,y) s ('.':xs) = go (x+1,y) s xs
    go (x,y) s ('#':xs) = go (x+1,y) (S.insert (x,y) s) xs
    go (x,y) s ('\n':xs) = go (0, y+1) s xs
    go (x,y) s ('^':xs) = go (x+1, y) (S.insert (x,y) s) xs
    go (x,y) s [] = (s, Nothing)
    go (x,y) s rest = (s, Just rest)

toAscii :: [Int] -> String
toAscii output = case error of
  Nothing -> 
    unlines $
    [ fmap toChar $ fmap (\x -> (x,y)) [minX..maxX] | y <- [minY..maxY] ] 
  Just err -> err
  
  where 
    minX = minimum $ S.map fst s
    minY = minimum $ S.map snd s
    maxX = maximum $ S.map fst s
    maxY = maximum $ S.map snd s

    toChar c =  if c `S.member` s then '#' else '.'

    (s, error) = toSet $ fmap chr output

fromAscii :: [String] -> [Int]
fromAscii asciiInput =
  (L.intercalate (fmap ord "\n") $ fmap (fmap ord) $ asciiInput) ++ [10]

test p = do
  (Right program) <- fmap (parse pProgram) $ readDay 21
  let (Halt output) = run program $ fromAscii p
  putStrLn $ toAscii output

{-  To solve this we run the function `test` with 
    different instructions and inspect the output.  -}

{- Initial Try  -}

w0 = [ "WALK "]

{- 
.................
.................
.................
#####@###########

We've fallen in to the first hold!
Let's try jumping:
* If there is a hole 1 space ahead
* There is ground 2 spaces ahead.

-}

w1 =  
    [ "NOT A J" -- J = !A
    , "AND B J" -- J = !A && B
    , "WALK" ]

{-
.................
.................
.................
#####@..#########

We haven't accounted for gaps of 3 spaces.
Let's try jumping if
  * Any of the next 3 spaces are holess;
  * The 4th space is not a hole.
-}

w2 =  [ "NOT A J" -- J = !A
      , "NOT B T" -- T = !B
      , "OR T J"  -- J = (!A || !B)
      , "NOT C T" -- T = !C
      , "OR T J"  -- J = (!A || !B || !C)
      , "NOT D T" -- T = !D
      , "NOT T T" -- T = D
      , "AND T J" -- T = (!A || !B || !C) && D
      , "WALK"]

{-  That seems to do it!  -}

-- Part Two

{-  Initial Try -}

r0 = [ "RUN" ]

{- 
.................
.................
.................
#####@###########

Fell at the first hurdle.
Let's try the same tactic as in part one.
-}

r1 =  [ "NOT A J"
      , "AND B J"
      , "RUN" ]

{-  
.................
.................
.................
#####@..#########

Let's try the solution to part one.
-}

r2 =  [ "NOT A J" -- J = !A
      , "NOT B T" -- T = !B
      , "OR T J"  -- J = (!A || !B)
      , "NOT C T" -- T = !C
      , "OR T J"  -- J = (!A || !B || !C)
      , "NOT D T" -- T = !D
      , "NOT T T" -- T = D
      , "AND T J" -- T = (!A || !B || !C) && D
      , "RUN"]

{-  
.................
.................
..@..............
#####.#.##.######
   ABCDEFGH
.................
...@.............
.................
#####.#.##.######

....@............
.................
.................
#####.#.##.######

.................
.....@...........
.................
#####.#.##.######

.................
.................
......@..........
#####.#.##.######

.................
.................
.................
#####.#@##.######

We jumped to soon!  
Let's try to delay the jump by checking 
that 5 spaces ahead is not a hole.
-}

r3 =  [ "NOT A J"
      , "NOT B T"
      , "OR T J" 
      , "NOT C T"
      , "OR T J" 
      , "NOT D T"
      , "NOT T T"
      , "AND T J" -- J = (!A || !B || !C) && D
      , "NOT E T" -- T = !E
      , "NOT T T" -- T = E
      , "AND T J" --  J = (!A || !B || !C) && D && E
      , "RUN"]

{-  
.................
.................
@................
#####.#..########

.................
.................
.@...............
#####.#..########
  
.................
.................
..@..............
#####.#..########

.................
.................
...@.............
#####.#..########

.................
.................
....@............
#####.#..########

.................
.................
.................
#####@#..########
  
D'oh!  We didn't jump.
Looking back at our last try, let's
try to delay the jump by insisting that either E or H
have ground.

That does it!
-}

r5 =  [ "NOT A J"
      , "NOT B T"
      , "OR T J" 
      , "NOT C T"
      , "OR T J" 
      , "NOT D T"
      , "NOT T T"
      , "AND T J" -- J = (!A || !B || !C) && D
      , "NOT E T" -- T = !E
      , "NOT T T" -- T = E
      , "OR H T" -- T = E OR F
      , "AND T J" --  J = (!A || !B || !C) && D && (E || H)
      , "RUN"]

partOne :: State -> Int
partOne program = last output
  where
    (Halt output) = 
      run program $ fromAscii w2

partTwo :: State - Int
partTwo program = last output
  where
    (Halt output) = 
      run program $ fromAscii r5

main :: IO ()
main = do
  (Right program) <- fmap (parse pProgram) $ readDay 21
  let (Halt output) = run program $ fromAscii r2
  putStrLn $ "Part One: " ++ (show $ partOne program)
  putStrLn $ "Part Two: " ++ (show $ partTwo program)