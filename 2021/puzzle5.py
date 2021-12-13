import re

def delta(x): return 0 if x == 0 else int(x/abs(x))

def draw_line(x, y, dest, grid):
    dx, dy = (delta(dest[0] - x), delta(dest[1] - y))

    while True:
        grid[y][x] += 1
        if (x,y) == dest: break
        x += dx
        y += dy

with open('puzzle5.dat') as f:
    grid = [[0 for x in range(1000)] for y in range(1000)]
    for line in f:
        m = re.match(r'(\d+),(\d+)\s\-\>\s(\d+),(\d+)', line.strip())
        draw_line(int(m.group(1)), int(m.group(2)), (int(m.group(3)), int(m.group(4))), grid)
    print(sum(sum(1 for x in row if x > 1) for row in grid))
