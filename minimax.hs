module Minimax where

import Common
import Data.Function (on)
import Data.List (maximumBy, minimumBy)
import Data.Map (Map, insert, unions)

data MTree b = Node {board :: b, val :: Int, children :: [MTree b]} deriving (Show)

class Minimax b where
  finished :: b -> Maybe Result

  possibleMoves :: Player -> b -> [b]

  minimax :: Player -> b -> Int
  minimax turn board =
    case finished board of
      Just result -> score result
      Nothing ->
        case turn of
          O -> maximum . map (minimax X) . possibleMoves O $ board
          X -> minimum . map (minimax O) . possibleMoves X $ board

  tree :: Player -> b -> MTree b
  tree turn board =
    case finished board of
      Just result -> Node{val = score result, children = [], board = board}
      Nothing -> Node{val = maximum (map val children), children = children, board = board}
        where
          children = map (tree (next turn)) (possibleMoves turn board)

  leaves :: MTree b -> Int
  leaves tree = (if null (children tree) then 1 else 0) + sum (map leaves (children tree))

  playAI :: Player -> b -> IO b
  playAI turn =
    do
      return
      . optimizeBy turn (compare `on` minimax (next turn))
      . possibleMoves turn
    where
      optimizeBy O = maximumBy
      optimizeBy X = minimumBy
