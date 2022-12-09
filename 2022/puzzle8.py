from functools import reduce

def getResults(puzzle, x, y):
    results = [(calc_view(puzzle, x, y, dx, dy)) for dx, dy in [(1, 0), (-1, 0), (0, 1), (0, -1)]]
    return any([r[0] for r in results]), reduce(lambda a, b: a * b, [r[1] for r in results])

def calc_view(puzzle, start_x, start_y, dx, dy):
    height = len(puzzle)
    width = len(puzzle[0])
    start_tree = puzzle[start_y][start_x]
    num_trees = 0
    x, y = start_x, start_y
    is_visible = 0

    while True:
        x += dx
        y += dy

        if x < 0 or x >= width or y < 0 or y >= height:
            is_visible = 1
            break

        num_trees += 1

        if puzzle[y][x] >= start_tree:
            break

    return is_visible, num_trees

with open("2022\\puzzle8.txt") as f:
    puzzle = f.read().splitlines()

grid = [getResults(puzzle, x, y) for x in range(len(puzzle[0])) for y in range(len(puzzle))]

# count the number of True values in vis_grid
print("Part 1: {}".format(sum(r[0] for r in grid)))

# find the max value in grid
print("Part 2: {}".format(max(r[1] for r in grid)))
