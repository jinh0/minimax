from common.game import Game, Board
from connectfour.board import full, at, place, finished_player

class ConnectFour(Game):
    @classmethod
    def finished(cls, board: Board) -> int | None:
        os, xs = board

        if (finished_player(os)): return 1
        elif (finished_player(xs)): return -1
        elif (os | xs).bit_count() == 42: return 0

        return None

    @classmethod
    def possibleMoves(cls, board: Board, move_ordering = False) -> list[Board]:
        boards = []

        for i in range(7):
            if not full(board, i):
                if cls.turn(board) == 1:
                    boards.append(place(board, i, 1))
                else:
                    boards.append(place(board, i, 0))

        return boards

    @classmethod
    def play_user(cls, board: Board) -> Board:
        coord = int(input("O's Turn: "))
        return place(board, coord, 1)

    @classmethod
    def print_board(cls, board: Board):
        for y in range(6):
            for x in range(7):
                print(at(board, x + (7 * y)), end=" ")
            print()
        print()

    @classmethod
    def parse(cls, board_str: str) -> Board:
        if board_str == 'empty':
            return (0, 0)

        board = (0, 0)
        for i, x in enumerate(board_str):
            # even indexed = O's turn (1); odd = X's turn (0)
            board = place(board, int(x) - 1, 1 if i % 2 == 0 else 0)

        return board

