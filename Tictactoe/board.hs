module Tictactoe.Board where

import Common
import Data.Bits
import Data.Int (Int16)

newtype Board = Board (Int16, Int16) deriving (Eq, Ord)

empty = Board (0, 0)

add :: Board -> (Int, Player) -> Board
add (Board (os, xs)) (coord, turn) = case turn of
  O -> Board (os .|. bit coord, xs)
  X -> Board (os, xs .|. bit coord)

at :: Board -> Int -> Maybe Player
at (Board (os, xs)) coord
  | (os .&. bit coord) > 0 = Just O
  | (xs .&. bit coord) > 0 = Just X
  | otherwise = Nothing

full :: Board -> Bool
full (Board (os, xs)) = popCount os + popCount xs == 9

elems :: [Int] -> Board -> [Maybe Player]
elems indices b = map (at b) indices

rows = allLines row where row n = elems [(n * 3) .. (n * 3) + 2]

cols = allLines col where col n = elems [n, n + 3, n + 6]

(dia1, dia2) = (elems [0, 4, 8], elems [2, 4, 6])

allLines line board = map (`line` board) [0 .. 2]

instance Show Board where
  show = unlines . map (unwords . map showPlayer) . rows
