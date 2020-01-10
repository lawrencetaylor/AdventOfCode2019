module Advent.Graph where

import           Advent.Input
import qualified Data.List    as L
import qualified Data.Maybe   as M
import qualified Data.Set     as S

data Queue a =
  Queue [a]
  deriving (Show)

empty :: Queue [a]
empty = Queue []

push :: a -> Queue a -> Queue a
push a (Queue q) = Queue (q ++ [a])

pushAll :: [a] -> Queue a -> Queue a
pushAll la q = foldr (push) q (reverse la)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue q) =
  case q of
    []     -> Nothing
    (x:xs) -> Just $ (x, Queue xs)

instance Foldable Queue where
  foldMap f (Queue q) = foldMap f (reverse q)

-- bfs :: (a -> [a])
-- bfs nextMoves
-- go :: (a -> [a]) -> (a ) S.Set r
{-
push current to Queue.
While queue is not empty
pop element of the queue
visit that element
push children of that element on to the queue
-}
testNext c 
  | length c > 4 = []
testNext c = [c ++ "A", c ++ "B"]


bfs :: (Ord b) => (a -> [a]) -> (a -> b) -> a -> [a]
bfs next rep a =
  M.catMaybes $
  L.unfoldr (bfsUnfold next) (S.empty, Queue [a])
  where
    bfsUnfold next (seen, q) =
      case pop q of
        Nothing -> Nothing
        Just (p, qs) ->
          case (rep p) `S.member` seen of
            True -> Just (Nothing, (seen, qs))
            False ->
              Just
                ( Just p
                , (S.insert (rep p) seen, pushAll (next p) qs))
