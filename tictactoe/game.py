from common.game import Game, Board
from tictactoe.board import finished_player, exists, at

class TicTacToe(Game):
    @classmethod
    def finished(cls, board: Board) -> int | None:
        os, xs = board

        if (finished_player(os)): return 1
        elif (finished_player(xs)): return -1
        elif (os | xs).bit_count() == 9: return 0

        return None

    @classmethod
    def possibleMoves(cls, board: Board) -> list[Board]:
        boards = []
        os, xs = board

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

