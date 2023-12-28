import Data.Bits
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (isNothing)

data Player = O | X deriving (Show, Eq)

type Board = (,) Int64 Int64

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
at (os, xs) x
  | (bit x .&. os) > 0 = Just O
  | (bit x .&. xs) > 0 = Just X
  | otherwise = Nothing

-- Showing and Printing
showPlayer (Just o) = show o
showPlayer Nothing = "_"

showRow (os, xs) n =
  unwords . map (showPlayer . at (os, xs)) $ [(n * 7) .. (n * 7) + 6]

showBoard :: Board -> String
showBoard board = unlines . map (showRow board) $ [0 .. 5]

printBoard = putStr . showBoard
