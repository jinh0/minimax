module Minimax where

import Common
import Data.Function (on)
import Data.List (maximumBy, minimumBy)

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

  playAI :: Player -> b -> IO b
  playAI turn =
    do
      return
      . optimizeBy turn (compare `on` minimax (next turn))
      . possibleMoves turn
    where
      optimizeBy O = maximumBy
      optimizeBy X = minimumBy