# Iterative Deepening Depth First Search
from common.game import Board, Strategy


class IDDFS(Strategy):
    def minimax(self, board: Board) -> int:
        return self.iddfs(board)

    def iddfs(self, board: Board) -> int:
