Board = tuple[int, int]

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
    return bool(
        ((player & 7) == 7) or ((player & 56) == 56) or ((player & 448) == 448) or # row
        ((player & 73) == 73) or ((player & 146) == 146) or ((player & 292) == 292) or # col
        ((player & 273) == 273) or ((player & 84) == 84) # diag
    )
