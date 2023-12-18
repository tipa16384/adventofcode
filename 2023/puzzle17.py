from timeit import Timer
from heapq import heappush, heappop
from math import inf

legal_moves = {(0, 0): ((1, 0), (0, 1)),
               (0, -1): ((1, 0), (-1, 0)),
               (1, 0): ((0, -1), (0, 1)),
               (0, 1): ((1, 0), (-1, 0)),
               (-1, 0): ((0, -1), (0, 1))}


def parts_is_parts(parts: str, mmin: int, mmax: int):
    with open('puzzle17.dat') as f:
        grid = [[int(x) for x in line] for line in f.read().splitlines()]
    destination_coord = (len(grid[0]) - 1, len(grid) - 1)
    heap = [(0, (0, 0), (0, 0))]
    heat_map = {(0, 0): 0}
    visited = set()

    while heap:
        heat_loss, coord, direction = heappop(heap)

        if coord == destination_coord:
            break

        if (coord, direction) in visited:
            continue

        visited.add((coord, direction))

        for new_direction in legal_moves[direction]:
            new_heat_loss = heat_loss
            for steps in range(1, mmax + 1):
                new_coord = (coord[0] + steps * new_direction[0],
                             coord[1] + steps * new_direction[1])
                if new_coord[0] < 0 or new_coord[1] < 0 \
                        or new_coord[0] > destination_coord[0] or new_coord[1] > destination_coord[1]:
                    continue
                new_heat_loss = new_heat_loss + \
                    grid[new_coord[1]][new_coord[0]]
                if steps >= mmin:
                    new_node = (new_coord, new_direction)
                    if heat_map.get(new_node, inf) <= new_heat_loss:
                        continue
                    heat_map[new_node] = new_heat_loss
                    heappush(heap, (new_heat_loss, new_coord, new_direction))

    print(parts, heat_loss)


t = Timer(lambda: parts_is_parts("Part 1:", 1, 3))
print("Part 1 executed in {}s".format(t.timeit(number=1)))
t = Timer(lambda: parts_is_parts("Part 2:", 4, 10))
print("Part 2 executed in {}s".format(t.timeit(number=1)))
