module Day23 where

import           Advent.Input
import           Advent.Intcode
import qualified Data.List       as L
import qualified Data.List.Split as L
import qualified Data.Map        as M
import qualified Data.Maybe      as M

type Address = Int
type Packet = (Int, Int)

data Computer =
  Computer
    { inputQueue :: [Int]
    , program    :: Exe
    }

type Message = (Address, Packet)

address :: Message -> Address
address = fst

type Computers = M.Map Int Computer

initial :: State -> Computers
initial s = computers
  where
    computers = M.fromList $ fmap (\a -> (a, Computer [a] (run s))) [0..49]

{-  Send messags from the computer at the specified address.  -}
send :: Computers -> Address -> (Computers, [Message])
send computers add = (newComputers, toSend)
  where
    c = computers M.! add
    (cInput, cQueue) =
      case inputQueue c of
        [] -> ([-1], [])
        q  -> ( q, [])
    Waiting exe output = (program c) $ cInput
    newC = c { inputQueue = cQueue, program = exe }
    newComputers = M.insert add newC computers
    toSend = fmap (\[x,a,b] -> (x,  (a,b))) $ L.chunksOf 3 $ output

{-  Send messages from all the computers on the network.  -}
sendAll :: Computers -> (Computers, [Message])
sendAll computers = foldl (\(c,p) n -> let (nc, np) = send c n in (nc, p ++ np)) (computers, []) [0..49]

{-  Recieve a sent packet.  -}
recieve :: (Address, Packet) -> Computers -> Computers
recieve (add, (x,y)) computers = M.insert add newC computers
  where
    c = computers M.! add
    newQ = (inputQueue c) ++ [x,y]
    newC = c { inputQueue = newQ }

{-  Run network until it is idle:
    * All computers have empty inputs;
    * All computers do not output messages when run.  -}
runUntilIdle :: (Computers, Maybe Message) -> (Computers, Maybe Message)
runUntilIdle (c, nat) =
  if( allInputsEmpty && outputsEmpty) then (c', nat)
  else runUntilIdle (c'', newNat)
  where
    (c', m') = sendAll c
    isNATMessage = (==) 255 . address
    newNat = foldl (\_ a -> Just a) nat $ filter isNATMessage m'
    c'' = foldl (flip recieve) c' $ filter (not . isNATMessage) m'

    allInputsEmpty = all (L.null . inputQueue) c
    outputsEmpty = L.null m'

{-  Create sequence of y values sent to 0th computer. -}
runAll :: Computers -> Maybe Message -> [Int]
runAll computers nat = [y] ++ (runAll c' $ mess)
  where
    (c, mess) = runUntilIdle (computers, nat)
    Just (_,(x,y)) = mess
    c0 = (c M.! 0) { inputQueue = [x,y]}
    c' = M.insert 0 c0 c

firstDupe :: [Int] -> Int
firstDupe (x:y:xs)
  | x == y = x
  | otherwise = firstDupe (y:xs)

partOne :: State -> Int
partOne s = head $ runAll init Nothing
  where
    init = initial s

partTwo :: State -> Int
partTwo s = firstDupe $ runAll init Nothing
  where
    init = initial s

main :: IO ()
main = do
  Right program <- parse pProgram <$> readDay 23
  putStrLn $ "Part One: " ++ (show $ partOne program)
  putStrLn $ "Part Two: " ++ (show $ partTwo program)