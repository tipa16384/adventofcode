from math import pow, sqrt
from collections import defaultdict

def get_g_score(node):
    global data, parent_nodes, start_node, g_score_cache
    if node == start_node:
        return get_weight(node)
    score = g_score_cache[parent_nodes[node]] + get_weight(node)
    g_score_cache[node] = score
    return score

def get_h_score(node):
    global end_node
    return sqrt(pow(end_node[0] - node[0], 2) + pow(end_node[1] - node[1], 2))

def get_f_score(node):
    return get_g_score(node) + get_h_score(node)

def get_weight(node):
    global data, rows, cols
    base = int(data[node[0] % rows][node[1] % cols])
    dist = node[0] // rows + node[1] // cols
    base += dist
    if base > 9:
        base -= 9
    return base

with open('puzzle15.dat', 'r') as f:
    data = [l.strip() for l in f.readlines()]

rows = len(data)
cols = len(data[0])

print (rows, cols)

start_node = (0,0)
end_node = (rows*5-1, cols*5-1)

open_nodes = [start_node]
closed_nodes = []
parent_nodes = defaultdict(lambda: None)
g_score_cache = defaultdict(lambda: float('inf'))
g_score_cache[start_node] = get_g_score(start_node)

#print (get_weight((1,1)))

count = 0

while open_nodes:
    if (count % 1000) == 0:
        print (len(open_nodes), len(closed_nodes))
    count += 1
    open_nodes.sort(key=get_f_score)
    current_node = open_nodes.pop(0)
    closed_nodes.append(current_node)
    if current_node == end_node:
        break
    for node in [(current_node[0]-1, current_node[1]), (current_node[0]+1, current_node[1]), (current_node[0], current_node[1]-1), (current_node[0], current_node[1]+1)]:
        if node[0] < 0 or node[1] < 0 or node[0] > end_node[0] or node[1] > end_node[1]:
            continue
        if node in closed_nodes:
            continue
        if node in open_nodes:
            if get_g_score(node) > get_g_score(current_node) + get_weight(node):
                parent_nodes[node] = current_node
        else:
            open_nodes.append(node)
            parent_nodes[node] = current_node

print (current_node)
risk_path = [current_node]
while current_node != start_node:
    current_node = parent_nodes[current_node]
    risk_path.append(current_node)
print (sum(get_weight(n) for n in risk_path if n != start_node))
