from itertools import combinations

def read_data() -> list:
    with open("puzzle11.dat") as f:
        return f.read().splitlines()

# make a list of the x,y coordinates of every '#' in the grid
def find_galaxies(grid: list) -> list:
    return [(x, y) for y, row in enumerate(grid) for x, galaxy in enumerate(row) if galaxy == '#']

# find the 'x' values for every column in grid consisting entirely of '.'
def find_empty_columns(grid: list) -> list:
    return [x for x in range(len(grid[0])) if all([row[x] == '.' for row in grid])]

# find the 'y' values for every row in grid consisting entirely of '.'
def find_empty_rows(grid: list) -> list:
    return [y for y, row in enumerate(grid) if all([galaxy == '.' for galaxy in row])]

# find the manhattan distance between two points
def manhattan_distance(p1: tuple, p2: tuple, empty_rows: list, empty_cols: list, multiplier:int) -> int:
    minx, maxx = min(p1[0], p2[0]), max(p1[0], p2[0])
    miny, maxy = min(p1[1], p2[1]), max(p1[1], p2[1])

    temp = (maxx - minx) + (maxy - miny)
    # for every empty row between the two points, add multiplier to the distance
    temp += multiplier * len([y for y in empty_rows if y > miny and y < maxy])
    # for every empty column between the two points, add multiplier to the distance
    temp += multiplier * len([x for x in empty_cols if x > minx and x < maxx])

    return temp

def parts_is_parts(multiplier:int = 2):
    grid = read_data()
    galaxies = find_galaxies(grid)
    empty_cols = find_empty_columns(grid)
    empty_rows = find_empty_rows(grid)
    # sum of all the manhattan distances between every pair of galaxies
    return sum([manhattan_distance(g1, g2, empty_rows, empty_cols, multiplier-1) for g1, g2 in combinations(galaxies, 2)])

print ("Part 1:", parts_is_parts())
print ("Part 2:", parts_is_parts(1000000))
