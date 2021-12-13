from collections import defaultdict

def read_connections():
    with open("puzzle12.dat") as f:
        connections = []
        for line in f:
            connections.append(tuple(line.strip().split("-")))
    return connections

def get_connection_map(connections):
    connection_map = defaultdict(list)
    for a, b in connections:
        connection_map[a].append(b)
        connection_map[b].append(a)
    return connection_map

def is_small_cave(cave):
    return cave[0] in 'abcdefghijklmnopqrstuvwxyz'

def find_unique_paths(start, end, visited, connection_map):
    if start in visited and is_small_cave(start):
        return
    visited.append(start)
    if start == end:
        yield visited
    for next_cave in connection_map[start]:
        yield from find_unique_paths(next_cave, end, visited.copy(), connection_map)

paths = [path for path in find_unique_paths(
    'start', 'end', list(), get_connection_map(read_connections()))]

print(len(paths))

connection_map = get_connection_map(read_connections())
small_caves = [cave for cave in connection_map if is_small_cave(
    cave) and cave != 'start' and cave != 'end']

unique_paths = set()
for cave in small_caves:
    connection_map = get_connection_map(read_connections())
    second_cave = cave + '-'
    connection_map[second_cave] = connection_map[cave]
    for other_cave in connection_map:
        if other_cave not in [cave, second_cave] and cave in connection_map[other_cave]:
            connection_map[other_cave].append(second_cave)
    paths = [path for path in find_unique_paths(
        'start', 'end', list(), connection_map)]
    for path in paths:
        path_string = ','.join(path).replace('-', '')
        unique_paths.add(path_string)

print (len(unique_paths))
