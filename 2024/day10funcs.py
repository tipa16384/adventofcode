import heapq
import re
from collections import defaultdict

def day10data(file) -> list:
    # return file as a list of lines
    return file.read().decode('utf-8').splitlines()

def day10parts(data):
    queue = []
    origin_score = defaultdict(set)
    trail_score = 0
    grid_height = len(data)
    grid_width = len(data[0])
    directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    for row, line in enumerate(data):
        for m in re.finditer(r'0', line):
            heapq.heappush(queue, ('0', row, m.start(), (row, m.start())))
    while queue:
        symbol, row, col, origin = heapq.heappop(queue)
        if symbol == '9':
            origin_score[origin].add((row, col))
            trail_score += 1
            continue
        for dy, dx in directions:
            new_row = row + dy
            new_col = col + dx
            if 0 <= new_row < grid_height and 0 <= new_col < grid_width:
                new_sym = data[new_row][new_col]
                if (ord(new_sym) - ord(symbol)) == 1:
                    heapq.heappush(queue, (new_sym, new_row, new_col, origin))
    return sum(len(x) for x in origin_score.values()), trail_score

