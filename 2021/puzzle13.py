import re
import curses

def read_input():
    first_points = None
    with open('puzzle13.dat') as f:
        points = set()
        for line in f:
            line = line.strip()
            m = re.match(r'fold along ([xy])=(\d+)', line)
            if m:
                points = fold(m.group(1), int(m.group(2)), points)
                first_points = first_points or len(points)
            elif line:
                points.add(tuple(map(int, line.split(','))))
    return first_points, points

def fold(axis, value, points):
    if axis == 'x':
        fn = lambda p: p if p[0] < value else (value - (p[0] - value), p[1])
    else:
        fn = lambda p: p if p[1] < value else (p[0], value - (p[1] - value))
    return set(fn(point) for point in points)

def draw(first_points, points):
    win = curses.initscr()
    win.clear()

    win.addstr(0, 0, 'Part 1: {}'.format(first_points))

    for p in points:
        win.addch(p[1]+2, p[0], '#')
    
    win.refresh()
    win.getch()
    curses.endwin()

fp, p = read_input()
draw(fp, p)
