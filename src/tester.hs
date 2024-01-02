module Tester where

import Common
import Control.Monad.Writer
import Minimax

minimax'' :: Minimax b => Player -> b -> (Int, Int)
minimax'' turn board =
  case finished board of
    Just result -> (score result, 1)
    Nothing ->
      case turn of
        O ->
          let children = map (minimax'' X) . possibleMoves O $ board
           in (maximum (map fst children), 1 + sum (map snd children))
        X ->
          let children = map (minimax'' O) . possibleMoves X $ board
           in (minimum (map fst children), 1 + sum (map snd children))
