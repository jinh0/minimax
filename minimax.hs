module Minimax where

import Common
import Data.Function (on)
import Data.List (maximumBy, minimumBy)
import Data.Map (Map, insert, unions)

class Minimax b where
  finished :: b -> Maybe Result
  possibleMoves :: Player -> b -> [b]

-- Play
playAI :: Minimax b => Player -> b -> IO b
playAI turn =
  do
    return
    . optimizeBy turn (compare `on` abSearch (next turn))
    . possibleMoves turn
  where
    optimizeBy O = maximumBy
    optimizeBy X = minimumBy

-- Minimax
minimax :: Minimax b => Player -> b -> Int
minimax turn board =
  case finished board of
    Just result -> score result
    Nothing ->
      case turn of
        O -> maximum . map (minimax X) . possibleMoves O $ board
        X -> minimum . map (minimax O) . possibleMoves X $ board

-- Minimax with Alpha Beta Pruning
abSearch :: Minimax b => Player -> b -> Int
abSearch = minimax' (-3) 3

minimax' :: Minimax b => Int -> Int -> Player -> b -> Int
minimax' alpha beta turn board =
  case finished board of
    Just result -> score result
    Nothing ->
      case turn of
        O -> maxval alpha beta board
        X -> minval alpha beta board

maxval alpha beta =
  maximum
    . takeWhile' (< beta)
    . scanl (\a board -> max a (minimax' a beta X board)) alpha
    . possibleMoves O

minval alpha beta =
  minimum
    . takeWhile' (> alpha)
    . scanl (\b board -> min b (minimax' alpha b O board)) beta
    . possibleMoves X

-- Inclusive takeWhile
takeWhile' _ [] = []
takeWhile' p (x : xs) = x : if p x then takeWhile' p xs else []
