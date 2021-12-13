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
    m = re.match(r'fold along ([xy])\=(\d+)', line)
    value = int(m.group(2))
    if m.group(1) == 'x':
        new_points = set(point if point[0] <= value else (
            value - (point[0] - value), point[1]) for point in points)
    else:
        new_points = set(point if point[1] <= value else (
            point[0], value - (point[1] - value)) for point in points)
    return new_points

points = read_input()
max_x = max(points, key=lambda x: x[0])[0]
max_y = max(points, key=lambda x: x[1])[1]
grid = [['#' if (x, y) in points else ' ' for x in range(max_x + 1)]
        for y in range(max_y + 1)]
for row in grid:
    print(''.join(row))
