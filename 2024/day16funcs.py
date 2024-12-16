import heapq
import time

directions = { 'N': (-1, 0), 'S': (1, 0), 'E': (0, 1), 'W': (0, -1) }
turns = { 'N': ['W', 'E'], 'S': ['E', 'W'], 'E': ['N', 'S'], 'W': ['S', 'N'] }

def day16data(file) -> list:
    return file.read().decode('utf-8').splitlines()

def find_position(data: list, char: str) -> tuple:
    for y, row in enumerate(data):
        if char in row:
            return y, row.index(char)
    return None

def part1(data: list) -> int:
    start_time = time.time()
    start = find_position(data, "S")
    direction = 'E'
    end = find_position(data, "E")

    visited = set()
    queue = []
    score = None

    heapq.heappush(queue, (0, start, direction))
    while queue:
        steps, pos, direction = heapq.heappop(queue)
        if pos == end:
            #print (path)
            score = steps
            break
        if (pos, direction) in visited:
            continue
        visited.add((pos, direction))

        for turn in turns[direction]:
            dy, dx = directions[turn]
            if data[pos[0] + dy][pos[1] + dx] != '#':
                heapq.heappush(queue, (steps + 1000, pos, turn))

        dy, dx = directions[direction]
        y, x = pos
        new_pos = (y + dy, x + dx)
        if data[new_pos[0]][new_pos[1]] != '#':
            heapq.heappush(queue, (steps + 1, new_pos, direction))

    print("Part 1 time taken: ", time.time() - start_time)
    return score

def part2(data: list, best_time) -> int:
    start_time = time.time()
    start = find_position(data, "S")
    direction = 'E'
    end = find_position(data, "E")
    visited_score_map = {}

    queue = []
    path_positions = set()

    heapq.heappush(queue, (0, start, direction, [start]))
    while queue:
        steps, pos, direction, path = heapq.heappop(queue)

        if steps <= best_time:
            if pos == end:
                for p in path: path_positions.add(p)
                continue

            if (pos, direction) not in visited_score_map or steps <= visited_score_map[(pos, direction)]:
                visited_score_map[(pos, direction)] = steps

                for turn in turns[direction]:
                    dy, dx = directions[turn]
                    if data[pos[0] + dy][pos[1] + dx] != '#':
                        heapq.heappush(queue, (steps + 1000, pos, turn, path))

                dy, dx = directions[direction]
                new_pos = (pos[0] + dy, pos[1] + dx)
                if data[new_pos[0]][new_pos[1]] != '#':
                    heapq.heappush(queue, (steps + 1, new_pos, direction, path + [new_pos]))

    print("Part 2 time taken: ", time.time() - start_time)

    return len(path_positions)
