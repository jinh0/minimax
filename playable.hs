module Playable where

import Common
import Minimax

class (Minimax b, Show b) => Playable b where
  play :: Player -> b -> IO b

  game :: Player -> b -> IO ()
  game turn board =
    case finished board of
      Just result -> putStrLn (show result ++ "!")
      Nothing -> do
        putStrLn (show turn ++ "'s turn:")
        board <- do
          if turn == O
            then play turn board
            else playAI turn board
        print board
        game (next turn) board
