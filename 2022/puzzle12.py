import heapq

def read_grid():
    with open(r"2022\puzzle12.txt") as f:
        return f.read().splitlines()

def value_of(c):
    match c:
        case 'S': return ord('a')
        case 'E': return ord('z')
        case _: return ord(c)

def legal_move(grid, x, y, mx, my):
    if mx < 0 or mx >= len(grid[0]) or my < 0 or my >= len(grid):
        return False
    return value_of(grid[my][mx]) <= value_of(grid[y][x]) + 1

def walk_the_path(grid, f):

    path = []
    backtrack = set()

    for move in [(x, y) for y, row in enumerate(grid) for x, c in enumerate(row) if f(c)]:
        heapq.heappush(path, (0, *move))

    while path:
        d, x, y = heapq.heappop(path)
        if grid[y][x] == "E":
            return d

        for mx, my in [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]:
            if legal_move(grid, x, y, mx, my) and (mx, my) not in backtrack:
                heapq.heappush(path, (d+1, mx, my))
                backtrack.add((mx, my))

grid = read_grid()

print ("Part 1:", walk_the_path(grid, lambda c: c == 'S'))
print ("Part 2:", walk_the_path(grid, lambda c: c == 'S' or c == 'a'))
