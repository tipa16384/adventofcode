import heapq


def get_weight(node):
    global data, rows, cols
    base = int(data[node[0] % rows][node[1] % cols]) + \
        node[0] // rows + node[1] // cols
    return base if base < 10 else base - 9


def path_find(end_node):
    start_node = (0, 0)

    open_nodes = []
    heapq.heappush(open_nodes, (0, start_node))
    closed_nodes = set()

    while open_nodes:
        weight, current_node = heapq.heappop(open_nodes)
        if current_node in closed_nodes:
            continue
        closed_nodes.add(current_node)

        if current_node == end_node:
            break
        for node in [(current_node[0]-1, current_node[1]), (current_node[0]+1, current_node[1]), (current_node[0], current_node[1]-1), (current_node[0], current_node[1]+1)]:
            if node[0] < 0 or node[1] < 0 or node[0] > end_node[0] or node[1] > end_node[1]:
                continue
            if node in closed_nodes:
                continue
            heapq.heappush(open_nodes, (get_weight(node) + weight, node))

    return weight


with open('puzzle15.dat', 'r') as f:
    data = [l.strip() for l in f.readlines()]

rows = len(data)
cols = len(data[0])

print('Part 1:', path_find((rows-1, cols-1)))
print('Part 2:', path_find((rows*5-1, cols*5-1)))
