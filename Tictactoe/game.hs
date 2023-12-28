module Tictactoe.Game (initGame) where

import Common
import Data.Function (on)
import Data.List (intercalate, maximumBy, minimumBy)
import Data.Maybe (isNothing)
import Minimax
import Playable
import Tictactoe.Board

instance Minimax Board where
  finished board
    | check O board = Just Win
    | check X board = Just Loss
    | length (tiles board) == 9 = Just Draw
    | otherwise = Nothing
    where
      check tile board =
        let allTile = all (== Just tile)
         in any allTile (rows board ++ cols board ++ [dia1 board, dia2 board])

  possibleMoves turn board =
    map (add board . (,turn))
      . filter (isNothing . (`lookup` tiles board))
      $ [0 .. 8]

instance Playable Board where
  play turn board = do
    coord <- getLine
    return (add board (read coord :: Int, turn))

initGame :: IO ()
initGame = do
  putStrLn "Tic Tac Toe"
  print empty
  game O empty
