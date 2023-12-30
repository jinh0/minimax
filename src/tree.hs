module Tree where

import Common
import Minimax

-- Game Tree Analysis

-- Game Tree
-- TODO: Is this a Functor?
newtype GTree b = Node [GTree b] deriving (Show)

tree :: Minimax b => Player -> b -> GTree b
tree turn board = case finished board of
  Just result -> Node []
  Nothing -> Node (map (tree (next turn)) (possibleMoves turn board))

leaves :: GTree b -> Int
leaves (Node children) =
  (if null children then 1 else 0) + sum (map leaves children)
