from connectfour.game import ConnectFour
from common.alphabeta import AlphaBeta
from common.classic import ClassicMinimax
from tictactoe.game import TicTacToe
from common.game import Playable

game = Playable(ConnectFour, AlphaBeta, debug=False)
l = game.benchmark('benchmark/connectfour/middle_easy.txt')
print('Max time taken:', max(l) * 1000)
print('Min time taken:', min(l) * 1000)
print('Avg time taken:', sum(l)/len(l) * 1000)
l.sort()
print('Median:', l[len(l)//2] * 1000)
