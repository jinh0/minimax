module Connectfour.Game (playGame) where

import Common
import Connectfour.Board
import Minimax
import Playable

instance Minimax Board where
  finished = undefined
  possibleMoves = undefined

instance Playable Board where
  play = undefined

playGame = initGame "Connectfour" empty
