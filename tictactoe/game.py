from common.game import Game, Board
from tictactoe.board import finished_player, exists, at, turn, wins

class TicTacToe(Game):
    @classmethod
    def finished(cls, board: Board) -> int | None:
        os, xs = board

        if (finished_player(os)): return 1
        elif (finished_player(xs)): return -1
        elif (os | xs).bit_count() == 9: return 0

        return None

    @classmethod
    def possibleMoves(cls, board: Board, move_ordering = False) -> list[Board]:
        boards = []
        os, xs = board

        if move_ordering:
            if turn(board) == 1:
                for w in wins:
                    # If O is 1 move or less away from winning AND the win does
                    # not intersect with X,
                    if (os & w).bit_count() >= 2 and (xs & w) == 0:
                        return [(os | w, xs)]
            else:
                for w in wins:
                    if (xs & w).bit_count() >= 2 and (os & w) == 0:
                        return [(os, xs | w)]


        for i in range(9):
            if not exists(board, i):
                if cls.turn(board) == 1:
                    boards.append((os | (1 << i), xs))
                else:
                    boards.append((os, xs | (1 << i)))

        return boards

    @classmethod
    def play_user(cls, board: Board) -> Board:
        coord = int(input("O's Turn: "))
        os, xs = board

        return (os | (1 << coord), xs)

    @classmethod
    def print_board(cls, board: Board):
        for y in range(3):
            for x in range(3):
                print(at(board, x + (3 * y)), end=" ")
            print()
        print()

    @classmethod
    def parse(cls, board_str: str) -> Board:
        if board_str == 'empty':
            return (0, 0)

        os, xs = 0, 0
        for i, x in enumerate(board_str):
            if i % 2 == 0:
                os |= 1 << int(x)
            else:
                xs |= 1 << int(x)

        return (os, xs)
