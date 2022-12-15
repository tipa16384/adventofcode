from time import time_ns
import re

def read_world():
    global all_world

    all_world = set()

    with open(r'2022\puzzle14.txt') as f:
        for l in f.read().splitlines():
            vertices = map(int, re.split(r' -> |,', l))
            x1, y1 = next(vertices), next(vertices)
            for x2, y2 in zip(*[iter(vertices)]*2):
                add_line_segment(x1, y1, x2, y2)
                x1, y1 = x2, y2

def add_line_segment(x1, y1, x2, y2):
    if x1 == x2:
        for y in range(min(y1, y2), max(y1, y2)+1):
            all_world.add((x1, y))
    else:
        for x in range(min(x1, x2), max(x1, x2)+1):
            all_world.add((x, y1))

def drop_sand(bounds, part2=False):
    x, y = 500, 0
    while True:
        if (x, y) in all_world or y > bounds:
            return False
        elif not (x, y + 1) in all_world:
            x, y = x, y + 1
        elif not (x - 1, y + 1) in all_world:
            x, y = x - 1, y + 1
        elif not (x + 1, y + 1) in all_world:
            x, y = x + 1, y + 1
        else:
            all_world.add((x, y))
            return True

for part2 in False, True:
    read_world()
    bounds = max(all_world, key=lambda k: k[1])[1]
    if part2:
        bounds += 2
        add_line_segment(500-bounds, bounds, 500+bounds, bounds)

    num_sand = 0
    start = time_ns()
    while drop_sand(bounds, part2): num_sand += 1

    print (f"{'Part 2' if part2 else 'Part 1'}: {num_sand} sand in {(time_ns() - start)/1e6}ms")
