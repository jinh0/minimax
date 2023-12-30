module Connectfour.Board where

import Common
import Data.Bits
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (fromJust, isNothing)

newtype Board = Board
  { tiles :: (Int64, Int64)
  }

empty = Board{tiles = (0, 0)}

add board turn col =
  case turn of
    O -> Board{tiles = (bit coord .|. os, xs)}
    X -> Board{tiles = (os, bit coord .|. xs)}
  where
    (os, xs) = tiles board
    coord = (fromJust (top board col) * 7) + col

top board x =
  find (isNothing . at board . (+ x) . (* 7)) $ reverse [0 .. 5]

full board = (popCount os + popCount xs) == 42
  where
    (os, xs) = tiles board

win turn board =
  isWinner row [0 .. 3] [0 .. 5]
    || isWinner col [0 .. 6] [0 .. 2]
    || isWinner diag [0 .. 3] [0 .. 2]
  where
    (os, xs) = tiles board
    player = if turn == O then os else xs
    isWinner line xx yy =
      any (\line -> (line .&. player) == line) [line (x, y) | x <- xx, y <- yy]
    row (x, y) = shiftL 15 (y * 7 + x)
    col (x, y) = shiftL 2113665 (y * 7 + x)
    diag (x, y) = shiftL 16843009 (y * 7 + x)

at :: Board -> Int -> Maybe Player
at board x
  | (bit x .&. os) > 0 = Just O
  | (bit x .&. xs) > 0 = Just X
  | otherwise = Nothing
  where
    (os, xs) = tiles board

-- Showing and Printing
showRow board n =
  unwords . map (showPlayer . at board) $ [(n * 7) .. (n * 7) + 6]

instance Show Board where
  show board = unlines . map (showRow board) $ [0 .. 5]
