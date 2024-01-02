Board = tuple[int, int]

def exists(board: Board, tile: int) -> bool:
    os, xs = board
    return bool(os & (1 << tile)) or bool(xs & (1 << tile))

def at(board: Board, coord: int):
    os, xs = board

    if bool(os & (1 << coord)): return 'O'
    if bool(xs & (1 << coord)): return 'X'
    return '_'

def full(board: Board, col: int) -> bool:
    return all(exists(board, col + 7 * row) for row in range(6))

def place(board: Board, col: int, turn: int) -> Board:
    os, xs = board

    y = 6
    for j in reversed(range(6)):
        if not exists(board, 7 * j + col):
            y = j
            break

    if turn == 1:
        return (os | (1 << (7 * y + col)), xs)
    else:
        return (os, xs | (1 << (7 * y + col)))

def finished_player(player: int) -> bool:
    row = 15
    col = 2113665
    diag = 16843009

    for y in range(6):
        for x in range(4):
            l = row << (y * 7 + x)

            if player & l == l:
                return True
            
    for y in range(3):
        for x in range(7):
            l = col << (y * 7 + x)

            if player & l == l:
                return True

    for y in range(3):
        for x in range(4):
            l = diag << (y * 7 + x)

            if player & l == l:
                return True

    return False
