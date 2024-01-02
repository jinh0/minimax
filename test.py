from common.alphabeta import AlphaBeta
from common.classic import ClassicMinimax
from tictactoe.game import TicTacToe
from common.game import Playable

game = Playable(TicTacToe, ClassicMinimax)
print(game.strategy.minimax((0, 0)))
print(game.strategy.cnt)
