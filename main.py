from common.alphabeta import AlphaBeta
from common.classic import ClassicMinimax
from tictactoe.game import TicTacToe
from connectfour.game import ConnectFour
from common.game import Playable

game = Playable(ConnectFour, AlphaBeta)
game.play()
