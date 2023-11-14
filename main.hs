data Tile = O | X deriving (Show, Eq)

type Board = [(Int, Tile)]

showBoard :: Board -> [Maybe Tile]
showBoard board = map (`lookup` board) [0 .. 8]

getLines line board = map (line board) [0..2]

rows = getLines row
  where
    row board n = map (`lookup` board) [(n * 3) .. (n * 3) + 2]

cols board = map (col board) [0 .. 2]
  where
    col board n = map (`lookup` board) [n, n + 3, n + 6]

dia1 board = map (`lookup` board) [0, 4, 8]

dia2 board = map (`lookup` board) [2, 4, 6]

value :: Tile -> Int
value O = 1
value X = 0

winner :: Board -> Maybe Tile
winner board
  | winner board O = Just O
  | winner board X = Just X
  | otherwise = Nothing
  where
    winner board tile =
      let ok = all (== Just tile)
       in any ok (rows board) || any ok (cols board) || ok (dia1 board) || ok (dia2 board)

-- minimax :: Board -> 
