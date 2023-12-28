module Connectfour.Game where

import Common
import Connectfour.Board
import Data.Maybe (isJust)
import Minimax
import Playable

instance Minimax Board where
  finished board
    | win O board = Just Win
    | win X board = Just Loss
    | full board = Just Draw
    | otherwise = Nothing

  possibleMoves :: Player -> Board -> [Board]
  possibleMoves turn board =
    map (add board turn)
      . filter (isJust . top board)
      $ [0 .. 6]

instance Playable Board where
  play :: Player -> Board -> IO Board
  play turn board = do
    col <- getLine
    return (add board turn (read col :: Int))

playGame = initGame "Connect Four" empty
