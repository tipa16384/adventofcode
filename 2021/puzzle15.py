from heapq import heappush, heappop
from timeit import default_timer as timer
from math import sqrt

def path_find(end_node):
    start_time = timer()

    open_nodes = []
    heappush(open_nodes, (0, 0, (0, 0)))
    closed_nodes = set()

    while open_nodes:
        _, weight, current_node = heappop(open_nodes)
        if current_node in closed_nodes:
            continue
        if current_node == end_node:
            break

        closed_nodes.add(current_node)

        for node in [(current_node[0]-1, current_node[1]), (current_node[0]+1, current_node[1]), 
                (current_node[0], current_node[1]-1), (current_node[0], current_node[1]+1)]:
            if node in closed_nodes or node[0] < 0 or node[1] < 0 or node[0] > end_node[0] or \
                    node[1] > end_node[1]:
                continue
            dy = node[0] - end_node[0]
            dx = node[1] - end_node[1]
            dist = sqrt(dx * dx + dy * dy)
            base = data[node[0] % rows][node[1] % cols] + node[0] // rows + node[1] // cols
            n_weight = (base if base < 10 else base - 9) + weight
            heappush(open_nodes, (n_weight + dist, n_weight, node))

    print(weight, timer() - start_time)

    return weight

with open('puzzle15.dat', 'r') as f: data = [[int(c) for c in l.strip()] for l in f.readlines()]

rows, cols = len(data), len(data[0])

print('Part 1:', path_find((rows-1, cols-1)))
print('Part 2:', path_find((rows*5-1, cols*5-1)))
