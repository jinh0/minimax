module Tictactoe.Board where

import Common

newtype Board = Board
  { tiles :: [(Int, Player)]
  }

empty = Board{tiles = []}

add board tile = Board{tiles = tile : tiles board}

elems indices b = map (`lookup` tiles b) indices

rows = allLines row where row n = elems [(n * 3) .. (n * 3) + 2]

cols = allLines col where col n = elems [n, n + 3, n + 6]

(dia1, dia2) = (elems [0, 4, 8], elems [2, 4, 6])

allLines line board = map (`line` board) [0 .. 2]

instance Show Board where
  show = unlines . map (unwords . map showPlayer) . rows
