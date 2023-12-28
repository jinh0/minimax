module Tictactoe.Board where

data Player = O | X deriving (Show, Eq)
data Result = Win | Loss | Draw deriving (Show)

type Board = [(Int, Player)]

elems :: [Int] -> Board -> [Maybe Player]
elems indices board = map (`lookup` board) indices

rows = allLines row where row n = elems [(n * 3) .. (n * 3) + 2]
cols = allLines col where col n = elems [n, n + 3, n + 6]
(dia1, dia2) = (elems [0, 4, 8], elems [2, 4, 6])

allLines line board = map (`line` board) [0 .. 2]

showBoard = unlines . map (unwords . map showTile) . rows
  where
    showTile Nothing = "_"
    showTile (Just O) = "O"
    showTile (Just X) = "X"

printBoard = putStrLn . showBoard
