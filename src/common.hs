module Common where

data Player = O | X deriving (Show, Eq)

next :: Player -> Player
next O = X
next X = O

showPlayer Nothing = "_"
showPlayer (Just O) = "O"
showPlayer (Just X) = "X"

data Result = Win | Loss | Draw deriving (Show)

score :: Result -> Int
score Win = 1
score Draw = 0
score Loss = -1
