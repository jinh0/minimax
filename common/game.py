from abc import ABC, abstractmethod

Board = tuple[int, int]

class Game(ABC):
    @classmethod
    @abstractmethod
    def finished(cls, board: Board) -> int | None:
        pass

    @classmethod
    @abstractmethod
    def possibleMoves(cls, board: Board, move_ordering = False) -> list[Board]:
        pass

    @classmethod
    @abstractmethod
    def play_user(cls, board: Board) -> Board:
        pass

    @classmethod
    @abstractmethod
    def print_board(cls, board: Board):
        pass

    @classmethod
    def turn(cls, board: Board) -> int:
        os, xs = board
        return ((os | xs).bit_count() + 1) % 2

class Strategy(ABC):
    cnt: int
    game: Game
    debug: bool

    def __init__(self, game, debug):
        self.game = game
        self.cnt = 0
        self.debug = debug

    def count(self):
        self.cnt += 1

        if self.cnt % 100_000 == 0:
            print("Cnt", self.cnt)

    @abstractmethod
    def minimax(self, board: Board) -> int:
        pass

class Playable:
    game: Game
    strategy: Strategy

    def __init__(self, game, strategy, debug=True):
        self.game = game
        self.strategy = strategy(game, debug)

    def play_ai(self, board: Board) -> Board:
        print("X's Turn:")
        return min(self.game.possibleMoves(board), key=self.strategy.minimax)

    def play(self):
        board = (0, 0)
        turn = 1

        self.game.print_board(board)

        while self.game.finished(board) is None:
            board = self.game.play_user(board) if turn == 1 else self.play_ai(board)
            turn = (turn + 1) % 2

            self.game.print_board(board)

        result = self.game.finished(board)
        if result == 1: print("You won!")
        elif result == -1: print("You lost!")
        else: print("Draw!")
