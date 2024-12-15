from functools import reduce
import re

pattern = re.compile(r'p\=([-\d]+),([-\d]+) v=([-\d]+),([-\d]+)')

def day14data(file):
    return [list(map(int, pattern.search(line).groups())) for line in file.read().decode('utf-8').splitlines()]

def part1(data, width, height, steps=100):
    quadrants = [0] * 4
    mid_width = width // 2
    mid_height = height // 2
    for px, py, vx, vy in data:
        endx = (px + vx * steps) % width
        endy = (py + vy * steps) % height
        if endx < mid_width and endy < mid_height:
            quadrants[0] += 1
        elif endx > mid_width and endy < mid_height:
            quadrants[1] += 1
        elif endx < mid_width and endy > mid_height:
            quadrants[2] += 1
        elif endx > mid_width and endy > mid_height:
            quadrants[3] += 1
    return reduce(lambda x, y: x * y, quadrants)

def part2(data, width, height, steps=100):
    steps = 0
    while True:
        bot_set = set()
        steps += 1
        for px, py, vx, vy in data:
            bot_set.add(((px + vx * steps) % width, (py + vy * steps) % height))
        # break if there is a contiguous horizontal line of bots at least ten long
        for y in range(height):
            line = ''.join('#' if (x, y) in bot_set else '.' for x in range(width))
            if '##########' in line:
                return steps
