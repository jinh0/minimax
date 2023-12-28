module Tictactoe.Game (initGame) where

import Common
import Data.Function (on)
import Data.List (intercalate, maximumBy, minimumBy)
import Data.Maybe (isNothing)
import Tictactoe.Board

-- Utility functions
finished :: Board -> Maybe Result
finished board
  | check O board = Just Win
  | check X board = Just Loss
  | length board == 9 = Just Draw
  | otherwise = Nothing
  where
    check tile board =
      let allTile = all (== Just tile)
       in any allTile (rows board ++ cols board ++ [dia1 board, dia2 board])

-- Minimax Algorithm:
-- Helper function: Possible moves
possibleMoves :: Player -> Board -> [Board]
possibleMoves turn board =
  map ((: board) . (,turn)) . filter (isNothing . (`lookup` board)) $ [0 .. 8]

minimax :: Player -> Board -> Int
minimax turn board =
  case finished board of
    Just result -> score result
    Nothing ->
      case turn of
        O -> maximum . map (minimax X) . possibleMoves O $ board
        X -> minimum . map (minimax O) . possibleMoves X $ board

-- Play the game
play :: Player -> Board -> IO Board
play turn board = do
  coord <- getLine
  return ((read coord :: Int, turn) : board)

playAI :: Player -> Board -> IO Board
playAI turn =
  do
    return
    . optimizeBy turn (compare `on` minimax (next turn))
    . possibleMoves turn
  where
    optimizeBy O = maximumBy
    optimizeBy X = minimumBy

game :: Player -> Board -> IO ()
game turn board =
  case finished board of
    Just result -> putStrLn (show result ++ "!")
    Nothing -> do
      putStrLn (show turn ++ "'s turn:")
      board <- do
        if turn == O
          then play turn board
          else playAI turn board
      printBoard board
      game (next turn) board

initGame :: IO ()
initGame = do
  putStrLn "Tic Tac Toe"
  printBoard []
  game O []
