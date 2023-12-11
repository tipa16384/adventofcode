from enum import Enum
from functools import lru_cache

Direction = Enum('Direction', ['NORTH', 'EAST', 'SOUTH', 'WEST'])

symbols = { '|' : [Direction.NORTH, Direction.SOUTH], 
            '-' : [Direction.EAST, Direction.WEST],
            'L' : [Direction.NORTH, Direction.EAST],
            'J' : [Direction.NORTH, Direction.WEST],
            '7' : [Direction.SOUTH, Direction.WEST],
            'F' : [Direction.SOUTH, Direction.EAST] }

offsets = { Direction.NORTH : (0, -1),
            Direction.EAST  : (1, 0),
            Direction.SOUTH : (0, 1),
            Direction.WEST  : (-1, 0) }

opposite_offset = { Direction.NORTH : Direction.SOUTH,
                    Direction.EAST  : Direction.WEST,
                    Direction.SOUTH : Direction.NORTH,
                    Direction.WEST  : Direction.EAST }

blocking_direction = { Direction.NORTH : Direction.EAST,
                       Direction.EAST  : Direction.SOUTH,
                       Direction.SOUTH : Direction.WEST,
                       Direction.WEST  : Direction.NORTH }

lgrid = None
lvisited = None

def read_data() -> list:
    with open("puzzle10.dat") as f:
        return f.read().splitlines()

def find_start():
    # start position is marked with 'S'.
    return [(row.index('S'), i) for i, row in enumerate(lgrid) if 'S' in row][0]

@lru_cache(maxsize=None)
def move_pos(pos, direction):
    # given a position (pos) and a direction, return the new position after moving in that direction
    offset = offsets[direction]
    return (pos[0] + offset[0], pos[1] + offset[1])

@lru_cache(maxsize=None)
def find_adjacent(pos: tuple) -> list:
    # given the grid and a position (pos), return the offsets of the two orthogonally adjacent squares that point back to the position
    # using symbols and offsets. if no adjacent squares are found, return empty list
    if pos[0] < 0 or pos[1] < 0 or pos[0] >= len(lgrid[0]) or pos[1] >= len(lgrid):
        return []
    symbol = lgrid[pos[1]][pos[0]]
    if symbol not in symbols and symbol != 'S':
        return []
    if symbol in symbols:
        return symbols[lgrid[pos[1]][pos[0]]]
    return [direction for direction in Direction \
            if opposite_offset[direction] in find_adjacent(move_pos(pos, direction))]

def part1() -> list:
    starting_position = find_start()
    adjacent = find_adjacent(starting_position)
    mice = [move_pos(starting_position, direction) for direction in adjacent]
    visited = [starting_position] + mice
    while True:
        new_mice = [move_pos(mouse, z) for mouse in mice for z in find_adjacent(mouse) if move_pos(mouse, z) not in visited]
        if len(new_mice) < 2:
            break
        visited += new_mice
        mice = new_mice
    return visited

@lru_cache(maxsize=None)
def blocked(direction: Direction, pos: tuple) -> bool:
    looking_at = lgrid[pos[1]][pos[0]]
    if looking_at not in symbols:
        return False
    return blocking_direction[direction] in symbols[looking_at]

# cache this
@lru_cache(maxsize=None)
def crossing_count(direction: Direction, pos: tuple) -> int:
    if pos[0] < 0 or pos[1] < 0 or pos[0] >= len(lgrid[0]) or pos[1] >= len(lgrid):
        return 0
    # print ("crossing_count", direction, pos)
    return (1 if pos in lvisited and blocked(direction, pos) else 0) + crossing_count(direction, move_pos(pos, direction))

@lru_cache(maxsize=None)
def is_inside(pos: tuple) -> bool:
    return not any((crossing_count(direction, pos) % 2) == 0 for direction in Direction)

def part2() -> int:
    # count the number of '.' in the grid that are not outside the maze
    return sum(1 for r, row in enumerate(lgrid) for x, c in enumerate(row) if is_inside((x, r)))

def what_symbol_is_s():
    start_pos = find_start()
    adjacent = find_adjacent(start_pos)
    # find symbol matching adjacent value
    for symbol, directions in symbols.items():
        if len(adjacent) == len(set(adjacent + directions)):
            return symbol
    return None

if __name__ == "__main__":
    lgrid = read_data()
    lvisited = part1()
    print ("Part 1:", (len(lvisited)-1)//2)
    # replace the 'S' in grid[start_pos[1]] with what_symbol_is_s(grid)
    start_pos = find_start()
    lgrid[start_pos[1]] = lgrid[start_pos[1]][:start_pos[0]] + what_symbol_is_s() + lgrid[start_pos[1]][start_pos[0]+1:]
    print ("Part 2:", part2())
