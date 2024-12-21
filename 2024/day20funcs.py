import heapq
from collections import defaultdict

def day20data(file):
    return file.read().decode('utf-8').splitlines()

def get_path(grid, start, end):
    pq = []
    visited = set()
    heapq.heappush(pq, (0, start, [start]))
    while pq:
        cost, current, path = heapq.heappop(pq)
        if current == end: break
        if current not in visited:
            visited.add(current)
            for neighbor in [(current[0]-1, current[1]), (current[0]+1, current[1]), (current[0], current[1]-1), (current[0], current[1]+1)]:
                if neighbor not in visited and grid[neighbor[0]][neighbor[1]] != '#':
                    heapq.heappush(pq, (cost+1, neighbor, path + [neighbor]))
    return path

def parts(grid, at_least, cheat_dist):
    start = next((y, x) for y, row in enumerate(grid) for x, cell in enumerate(row) if cell == 'S')
    end = next((y, x) for y, row in enumerate(grid) for x, cell in enumerate(row) if cell == 'E')

    path = get_path(grid, start, end)
    cheats = defaultdict(int)

    for i, cell in enumerate(path):
        for j, dest_cell in enumerate(path[i + at_least:], i + at_least):
            dist = abs(cell[0] - dest_cell[0]) + abs(cell[1] - dest_cell[1])
            if 2 <= dist <= cheat_dist:
                savings = j - i - dist
                if savings >= at_least:
                    cheats[savings] += 1

    return sum(cheats.values())
