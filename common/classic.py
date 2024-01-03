from common.game import Board, Strategy
from functools import cache


class ClassicMinimax(Strategy):
    @cache
    def minimax(self, board: Board) -> int:
        if self.debug:
            super().count()

        if (f := self.game.finished(board)) is not None:
            return f

        moves = (self.minimax(b) for b in self.game.possibleMoves(board))

        if self.game.turn(board) == 1:
            return max(moves)
        else:
            return min(moves)

