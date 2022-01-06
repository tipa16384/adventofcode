import heapq
from functools import lru_cache
from timeit import default_timer as timer

def read_data() -> list:
    with open('puzzle23.dat') as f:
        return f.read().split('\n')

move_values = {'A': 1, 'B':10, 'C':100, 'D':1000 }
home_x = {'A': 3, 'B': 5, 'C': 7, 'D': 9}

def marshall(data: list) -> str:
    return '|'.join(row for row in data)

@lru_cache(maxsize=None)
def unmarshall(key: str) -> list:
    return list(row for row in key.split('|'))

@lru_cache(maxsize=None)
def calc_dist(key: str) -> int:
    data = unmarshall(key)
    total_dist = 0

    for x, c in enumerate(data[1]):
        if c in move_values.keys():
            total_dist += (abs(x - home_x[c]) + 1) * move_values[c]
    
    for y, row in enumerate(data[2:]):
        for x, c in enumerate(row):
            if c in move_values.keys() and x != home_x[c]:
                total_dist += (2 + y + abs(x - home_x[c])) * move_values[c]
    
    return total_dist

def can_go_home(key: str, c: str) -> bool:
    data = unmarshall(key)
    for row in data[2:]:
        occupant = row[home_x[c]]
        if occupant in move_values.keys() and occupant != c:
            #print (f"{c} can't go home, {occupant} is in the house")
            return False
    #print (f"{c} can go home, no trespassers")
    return True

def hallway_not_blocked(key: str, c: str, x: int) -> bool:
    data = unmarshall(key)
    step = -1 if x > home_x[c] else 1
    while x != home_x[c]:
        x += step
        if data[1][x] != '.':
            #print (f"{c} can't go home, hallway blocked at {x}")
            return False
    #print (f"{c} can go home, hallway not blocked")
    return True

def yield_moves(key: str) -> None:
    data = unmarshall(key)
    for x, c in enumerate(data[1]):
        if c in move_values.keys() and can_go_home(key, c) and hallway_not_blocked(key, c, x):
            dist = abs(x - home_x[c])
            dest_y = 1
            for y, row in enumerate(data[2:]):
                if row[home_x[c]] == '.':
                    dist += 1
                    dest_y = y + 2

            #print (f"move {c} from {x},1 to {home_x[c]},{dest_y}")
            yield (x, 1, home_x[c], dest_y, dist * move_values[c])
    
    for y in range(2, len(data)):
        for x in home_x.values():
            c = data[y][x]
            if c in move_values.keys() and (x != home_x[c] or not can_go_home(key, c)):
                if any(row[x] != '.' for row in data[1:y]):
                    continue
                dx = x - 1
                while data[1][dx] == '.':
                    if dx not in home_x.values():
                        dist = y - 1 + x - dx
                        #print (f"move {c} from {x},{y} into the hallway at {dx},1")
                        yield (x, y, dx, 1, dist * move_values[c])
                    dx -= 1
                dx = x + 1
                while data[1][dx] == '.':
                    if dx not in home_x.values():
                        dist = y - 1 + dx - x
                        #print (f"move {c} from {x},{y} into the hallway at {dx},1")
                        yield (x, y, dx, 1, dist * move_values[c])
                    dx += 1

@lru_cache(maxsize=None)
def make_move(key: str, move: tuple) -> str:
    data = unmarshall(key)
    new_data = list(data)
    s = list(c for c in data[move[1]])
    s[move[0]] = '.'
    new_data[move[1]] = ''.join(s)
    s = list(c for c in data[move[3]])
    s[move[2]] = data[move[1]][move[0]]
    new_data[move[3]] = ''.join(s)
    return marshall(new_data)

def print_board(key: str) -> None:
    data = unmarshall(key)
    for row in data:
        print (''.join(row))
    print ()

data = marshall(read_data())

open_nodes = list()
heapq.heappush(open_nodes, (0, 0, data, []))
nodes_seen = 0

start = timer()

while open_nodes:
    nodes_seen += 1
    score, total_dist, board, moves = heapq.heappop(open_nodes)
    #print (f"{score} {total_dist} {len(moves)}")
    dist = calc_dist(board)
    if dist == 0:
        for h in moves:
            print_board(h)
        print_board(board)
        print (f"Score: {total_dist} nodes: {nodes_seen} time: {timer() - start}")
        break
    history = list(moves)
    history.append(board)
    for m in yield_moves(board):
        next_board = make_move(board, m)
        move_dist = m[4]
        next_score = score + move_dist + calc_dist(next_board)
        heapq.heappush(open_nodes, (next_score, total_dist + move_dist, next_board, history))
