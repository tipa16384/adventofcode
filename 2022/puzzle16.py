from time import time_ns
import re
from collections import defaultdict


class Node:
    def __init__(self, data):
        self.name = data[0]
        self.flow = int(data[1])
        self.neighbor_map = defaultdict(int)
        for neighbor in data[2:]:
            self.neighbor_map[neighbor] = 1

    def __repr__(self):
        return f"Node({self.name}, {self.flow}, {self.neighbor_map})\n"


def read_input():
    nodes = []

    with open(r"2022\puzzle16.txt") as f:
        for l in f.read().splitlines():
            nodes.append(Node(re.findall(r"[A-Z][A-Z]|\d+", l)))

    for node in nodes:
        for a_node in nodes:
            if node != a_node and node.name in a_node.neighbor_map:
                current_distance = a_node.neighbor_map[node.name]
                for my_neighbor in node.neighbor_map:
                    if my_neighbor == a_node.name:
                        continue
                    if my_neighbor not in a_node.neighbor_map:
                        a_node.neighbor_map[my_neighbor] = current_distance + \
                            node.neighbor_map[my_neighbor]
                    else:
                        a_node.neighbor_map[my_neighbor] = min(
                            a_node.neighbor_map[my_neighbor], current_distance + node.neighbor_map[my_neighbor])
                if not node.flow:
                    del a_node.neighbor_map[node.name]

    return {node.name: node for node in nodes if node.name == starting_node or node.flow}


def traverse(node_map, current_node, visited=[], time=0, max_time=30):
    if time >= max_time:
        return 0, visited

    node = node_map[current_node]
    score = node.flow * (max_time - time)
    new_visited = visited + [current_node]

    child_scores = max([traverse(node_map, neighbor, new_visited, time + node.neighbor_map[neighbor] + 1, max_time)
                        for neighbor in node.neighbor_map if neighbor not in visited])

    return (score + child_scores[0], [current_node] + child_scores[1])


def part1():
    score, _ = traverse(node_map, starting_node)
    return score


def part2():
    score, path = traverse(node_map, starting_node, max_time=26)

    # remove the nodes in 'path' from the neighbor_map of all nodes
    for node in node_map.values():
        for p in path[1:]:
            if p in node.neighbor_map:
                del node.neighbor_map[p]

    elephant_score, _ = traverse(node_map, starting_node, max_time=26)
    return elephant_score+score


starting_node = 'AA'
node_map = read_input()

start = time_ns()
print(f"Part 1: {part1()} in {(time_ns()-start)/1e6}ms")
start = time_ns()
print(f"Part 2: {part2()} in {(time_ns()-start)/1e6}ms")
