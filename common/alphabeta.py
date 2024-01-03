from functools import cache
from common.game import Board, Strategy

class AlphaBeta(Strategy):
    def minimax(self, board: Board) -> int:
        return self.ab_search(board)

    @cache
    def ab_search(self, board: Board, alpha = -1, beta = 1) -> int:
        if self.debug:
            super().count()

        if (f := self.game.finished(board)) is not None:
            return f

        if self.game.turn(board) == 1:
            return self.maxval(board, alpha, beta)
        else:
            return self.minval(board, alpha, beta)

    def maxval(self, board: Board, alpha: int, beta: int):
        val = -1
        for next_board in self.game.possibleMoves(board, True):
            val = max(val, self.ab_search(next_board, alpha, beta))
            alpha = max(val, alpha)

            if val >= beta:
                return val

        return val

    def minval(self, board: Board, alpha: int, beta: int):
        val = 1
        for next_board in self.game.possibleMoves(board, True):
            val = min(val, self.ab_search(next_board, alpha, beta))
            beta = min(val, beta)

            if val <= alpha:
                return val

        return val
