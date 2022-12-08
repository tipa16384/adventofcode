from functools import reduce

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

dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]

with open("2022\\puzzle8.txt") as f:
    puzzle = f.read().splitlines()

width = len(puzzle[0])
height = len(puzzle)

# make grid of height and width
vis_grid = [[False for _ in range(width)] for _ in range(height)]

# make int grid of height and width
grid = [[1 for _ in range(width)] for _ in range(height)]

# for each cell in puzzle
for y in range(height):
    for x in range(width):
        results = [(calc_view(puzzle, x, y, dx, dy)) for dx, dy in dirs]
        vis_grid[y][x] = any([r[0] for r in results])
        grid[y][x] = reduce(lambda x, y: x * y, [r[1] for r in results])


# count the number of True values in vis_grid
print("Part 1: {}".format(sum([sum(row) for row in vis_grid])))

# find the max value in grid
print("Part 2: {}".format(max([max(row) for row in grid])))
