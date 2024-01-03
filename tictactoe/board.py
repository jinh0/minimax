Board = tuple[int, int]

# Configurations for a winning line
wins = [
    7, 56, 448, # row
    73, 146, 292, # col
    273, 84 # diag
]

def exists(board: Board, tile: int) -> bool:
    os, xs = board

    return bool(os & (1 << tile)) or bool(xs & (1 << tile))

def at(board: Board, coord: int):
    os, xs = board

    if bool(os & (1 << coord)): return 'O'
    if bool(xs & (1 << coord)): return 'X'
    return '_'

def turn(board: Board) -> int:
    os, xs = board
    return ((os | xs).bit_count() + 1) % 2

def finished_player(player: int) -> bool:
    return any((player & win) == win for win in wins)
