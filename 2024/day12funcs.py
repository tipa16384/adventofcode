from collections import defaultdict

directions = [(0, -1), (1, 0), (0, 1), (-1, 0)]

def day12data(file):
    data = ['.' + line + '.' for line in file.read().decode('utf-8').splitlines()]
    border = '.' * len(data[0])
    data.insert(0, border)
    data.append(border)
    return data

def day12parts(data, pfunc):
    total_cost = 0
    flooded_region = set()

    for y, row in enumerate(data):
        for x, cell in enumerate(row):
            if cell != '.' and (x, y) not in flooded_region:
                flood_region = set()
                flood(cell, x, y, data, flood_region)
                flooded_region.update(flood_region)
                total_cost += len(flood_region) * pfunc(flood_region)

    return total_cost

def flood(c, x, y, data, flood_region):
    if ((x, y) not in flood_region) and data[y][x] == c:
        flood_region.add((x, y))
        for dx, dy in directions:
            flood(c, x + dx, y + dy, data, flood_region)

def measure_perimeter(flood_region: set) -> int:
    perimeter = 0
    for x, y in flood_region:
        for dx, dy in directions:
            if (x + dx, y + dy) not in flood_region:
                perimeter += 1
    return perimeter

def measure_sides(flood_region: set) -> int:
    side_lists = [defaultdict(list) for _ in range(len(directions))]

    for x, y in flood_region:
        for i, (dx, dy) in enumerate(directions):
            if (x + dx, y + dy) not in flood_region:
                side_lists[i][x if dx else y].append(y if dx else x)

    num_sides = 0
    for sides in side_lists:
        for values in sides.values():
            values.sort()
            num_sides += sum(
                values[i] - values[i - 1] > 1 for i in range(1, len(values))
            ) + 1

    return num_sides
