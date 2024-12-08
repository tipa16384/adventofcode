from collections import defaultdict
from itertools import combinations
from math import gcd

def day8_data(file) -> dict:
    list_of_lines = file.read().decode('utf-8').splitlines()
    data = defaultdict(set)
    for y, line in enumerate(list_of_lines):
        for x, element in enumerate(line):
            if element != '.':
                data[element].add((y, x))
    return (data, len(list_of_lines[0]), len(list_of_lines))

def day8part1(data: dict, grid_height: int, grid_width: int) -> int:
    antinodes = set()
    for locations in data.values():
        for (y1, x1), (y2, x2) in combinations(locations, 2):
            dx, dy = x2 - x1, y2 - y1
            potential_antinodes = { (y1 - dy, x1 - dx), (y2 + dy, x2 + dx) }
            antinodes.update((y, x) for y, x in potential_antinodes
                if 0 <= y < grid_height and 0 <= x < grid_width)
    return len(antinodes)

def day8part2(data: dict, grid_height: int, grid_width: int) -> int:
    antinodes = set()
    for locations in data.values():
        for (y1, x1), (y2, x2) in combinations(locations, 2):
            dx, dy = x2 - x1, y2 - y1
            dv = gcd(dx, dy)
            dx //= dv
            dy //= dv
            while 0 <= y2 < grid_height and 0 <= x2 < grid_width:
                antinodes.add((y2, x2))
                y2 += dy
                x2 += dx
            while 0 <= y1 < grid_height and 0 <= x1 < grid_width:
                antinodes.add((y1, x1))
                y1 -= dy
                x1 -= dx
    return len(antinodes)
