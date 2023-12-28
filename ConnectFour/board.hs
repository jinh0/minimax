module Connectfour.Board where

import Common
import Data.Bits
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (isNothing)

newtype Board = Board
  { tiles :: (Int64, Int64)
  }

empty = Board{tiles = (0, 0)}

col board x =
  find (isNothing . at board . (+ x) . (* 7)) $ reverse [0 .. 5]

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
