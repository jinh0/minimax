import Data.List (intercalate, maximumBy, minimumBy)
import Data.Maybe (isNothing)
import Data.Function (on)

data Tile = O | X deriving (Show, Eq)
data Result = Win | Loss | Draw deriving (Show)

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
score :: Result -> Int
score Win = 1
score Draw = 0
score Loss = -1

turn :: Board -> Tile
turn board =
  if numTiles O board == numTiles X board then O else X
  where numTiles tile = length . filter ((== tile) . snd)

next :: Tile -> Tile
next O = X
next X = O

finished :: Board -> Maybe Result
finished board
  | check O board     = Just Win
  | check X board     = Just Loss
  | length board == 9 = Just Draw
  | otherwise         = Nothing
  where
    check tile board =
      let allTile = all (== Just tile) in
      any allTile (rows board ++ cols board ++ [dia1 board, dia2 board])


-- Minimax Algorithm:
-- Helper function: Possible moves
possibleMoves :: Tile -> Board -> [Board]
possibleMoves turn board =
  map ((: board) . (, turn)) . filter (isNothing . (`lookup` board)) $ [0 .. 8]

minimax :: Tile -> Board -> Int 
minimax turn board =
  case finished board of
    Just result -> score result
    Nothing ->
      case turn of
        O -> maximum . map (minimax X) $ possibleMoves O board
        X -> minimum . map (minimax O) $ possibleMoves X board


-- Play the game
play :: Tile -> Board -> IO Board
play turn board = do
  coord <- getLine
  return ((read coord :: Int, turn) : board)

playAI :: Tile -> Board -> IO Board
playAI X = do
  return
  . minimumBy (compare `on` minimax O)
  . possibleMoves X

game :: Tile -> Board -> IO ()
game turn board =
  case finished board of
    Just result -> putStrLn (show result ++ "!")
    Nothing -> do
      putStrLn (show turn ++ "'s turn:")
      board <- do 
        if turn == O then play turn board
        else playAI turn board
      printBoard board
      game (next turn) board

main :: IO ()
main = do
  putStrLn "Tic Tac Toe"
  printBoard []
  game O []
