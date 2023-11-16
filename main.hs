import Data.List (intercalate)
import Data.Maybe (isNothing)

data Tile = O | X deriving (Show, Eq)

type Board = [(Int, Tile)]

-- Access elements
elems :: [Int] -> Board -> [Maybe Tile]
elems indices board = map (`lookup` board) indices

allLines line board = map (`line` board) [0 .. 2]

rows = allLines row where row n = elems [(n * 3) .. (n * 3) + 2]
cols = allLines col where col n = elems [n, n + 3, n + 6]

(dia1, dia2) = (elems [0, 4, 8], elems [2, 4, 6])

-- Display fns
showBoard :: Board -> String
showBoard = unlines . map (unwords . map showTile) . rows
  where
    showTile Nothing  = "_"
    showTile (Just O) = "O"
    showTile (Just X) = "X"

printBoard = putStr . showBoard


-- Utility functions
value :: Tile -> Int
value O = 1
value X = -1

turn :: Board -> Tile
turn board =
  if numTiles O board == numTiles X board then O else X
  where numTiles tile = length . filter ((== tile) . snd)

nextTurn O = X
nextTurn X = O

winner :: Board -> Maybe Tile
winner board
  | check O board = Just O
  | check X board = Just X
  | otherwise     = Nothing
  where
    check tile board =
      let allTile = all (== Just tile)
       in any allTile (rows board ++ cols board ++ [dia1 board, dia2 board])


-- Minimax Algorithm:
-- Helper function: Possible moves
possibleMoves :: Tile -> Board -> [Board]
possibleMoves turn board =
  map ((: board) . (, turn)) . filter (isNothing . (`lookup` board)) $ [0 .. 8]

minimax :: Board -> Int
minimax board =
  case winner board of
    Just t -> value t
    Nothing ->
      case turn board of
        O -> (maximum . map minimax) (possibleMoves O board)
        X -> (minimum . map minimax) (possibleMoves X board)


-- Play the game
play turn board = do
  coord <- getLine
  return ((read coord :: Int, turn) : board)

game turn board =
  case winner board of
    Just t -> putStrLn (show t ++ " won!")
    Nothing -> do
      putStrLn (show turn ++ "'s turn:")
      board <- play turn board
      printBoard board
      game (nextTurn turn) board

main :: IO ()
main = do
  putStrLn "Tic Tac Toe"
  game O []
