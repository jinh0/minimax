module Connectfour.Game where

import Common
import Data.Bits
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (isNothing)

newtype Board = Board
  { tiles :: (Int64, Int64)
  }

-- Play
col board j =
  find (\y -> isNothing (at board (y * 7 + j))) $ reverse [0 .. 5]

playCol board col = error

-- play :: Player -> Board -> IO Board
play player board = do
  col <- getLine
  return error

-- play player . playCol board (read col :: Int)

at :: Board -> Int -> Maybe Player
at (Board (os, xs)) x
  | (bit x .&. os) > 0 = Just O
  | (bit x .&. xs) > 0 = Just X
  | otherwise = Nothing

-- Showing and Printing
showRow board n =
  unwords . map (showPlayer . at board) $ [(n * 7) .. (n * 7) + 6]

instance Show Board where
  show board = unlines . map (showRow board) $ [0 .. 5]
