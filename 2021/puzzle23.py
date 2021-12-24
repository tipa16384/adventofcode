from collections import defaultdict

data = "##############...........####D#C#D#B###  #C#A#A#B#    #########  "

solved = "##############...........####A#B#C#D###  #A#B#C#D#    #########  "
##########00000000001111111111222222222233333333334444444444555555555566666
##########01234567890123456789012345678901234567890123456789012345678901234

move_nrg = defaultdict(lambda: 100000000000)
row_len = 13

move_cost = { 'A': 1, 'B': 10, 'C': 100, 'D': 1000 }

def in_hallway(pos: int) -> bool: return 14 <= pos <= 24

def is_home(current: str, x: chr, pos: int) -> bool:
    if x == 'A' and pos == 42 or (pos == 29 and current[42] == 'A'):
        return True
    if x == 'B' and pos == 44 or (pos == 31 and current[44] == 'B'):
        return True
    if x == 'C' and pos == 46 or (pos == 33 and current[46] == 'C'):
        return True
    if x == 'D' and pos == 48 or (pos == 35 and current[48] == 'D'):
        return True
    return False

def get_moves(current: str, pos: int, x: chr) -> list:
    if in_hallway(pos):
        pass
    
def make_move(current: str, nrg: int) -> str:
    global move_nrg
    if move_nrg[current] <= nrg:
        return current
    move_nrg[current] = nrg
    if current == solved:
        return current
    # make all possible moves
    for pos, x in enumerate(current):
        if x in 'ABCD' and not is_home(current, x, pos):
            moves = get_moves(current, pos, x)
            for move, cost in moves:
                new_current = make_move(move, nrg + cost * move_cost[x])
                if new_current == solved:
                    return new_current

make_move(data, 0)

print (f"Part 1: {move_nrg[solved]}")
