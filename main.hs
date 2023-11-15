data Tile = O | X deriving (Show, Eq)

type Board = [(Int, Tile)]

elems :: [Int] -> Board -> [Maybe Tile]
elems indices board = map (`lookup` board) indices

getLines line board = map (`line` board) [0 .. 2]

rows = getLines row
  where row n = elems [(n * 3) .. (n * 3) + 2]

cols = getLines col
  where col n = elems [n, n + 3, n + 6]

dia1 = elems [0, 4, 8]

dia2 = elems [2, 4, 6]

-- showBoard :: Board -> [String]
printBoard board =
  map (showTile . (`lookup` board)) [0 .. 8]
  where
    showTile Nothing = "_"
    showTile (Just O) = "O"
    showTile (Just X) = "X"

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
