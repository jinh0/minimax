from common.alphabeta import AlphaBeta
from common.classic import ClassicMinimax
from tictactoe.game import TicTacToe
from common.game import Playable

game = Playable(TicTacToe, AlphaBeta, debug=False)
game.benchmark('benchmark/tictactoe.txt')
