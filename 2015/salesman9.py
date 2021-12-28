from itertools import permutations
from collections import defaultdict

def read_data():
    nodes = defaultdict(list)
    with open('salesman9.dat') as f:
        for line in f.read().split('\n'):
            node_name, dest = line.split(" to ")
            dest_name, dist = dest.split(" = ")
            nodes[node_name].append((dest_name, int(dist)))
            nodes[dest_name].append((node_name, int(dist)))
    return nodes

def length(nodes, source, dest):
    for x, y in nodes[source]:
        if x == dest:
            return y

def sum_lengths(nodes):
    return (sum(length(nodes, x, y) for x, y in zip(path, path[1:])) for path in permutations(nodes))

nodes = read_data()
print("Part 1:", min(sum_lengths(nodes)))
print("Part 2:", max(sum_lengths(nodes)))
