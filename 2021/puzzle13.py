import re

def read_input():
    with open('puzzle13.dat') as f:
        points = set()
        for line in f:
            line = line.strip()
            if line.startswith('fold'):
                points = fold(line, points)
            elif line:
                points.add(tuple(map(int, line.split(','))))
    return points

def fold(line, points):
    m = re.match(r'fold along ([xy])=(\d+)', line)
    value = int(m.group(2))
    if m.group(1) == 'x':
        fn = lambda p: p if p[0] < value else (value - (p[0] - value), p[1])
    else:
        fn = lambda p: p if p[1] < value else (p[0], value - (p[1] - value))
    return set(fn(point) for point in points)

points = read_input()
max_x = max(points, key=lambda x: x[0])[0]
max_y = max(points, key=lambda x: x[1])[1]

grid = [['#' if (x, y) in points else ' ' for x in range(max_x + 1)]
        for y in range(max_y + 1)]

for row in grid:
    print(''.join(row))
