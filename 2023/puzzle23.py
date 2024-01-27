from heapq import heappush, heappop
from sys import maxsize

directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]
steep_map = { '>': [(1,0)], '<': [(-1,0)], '^': [(0,-1)], 'v': [(0,1)] }

def read_data():
    with open("puzzle23.dat") as f:
        return f.read().splitlines()

def part1(data: list):
    heap = []
    visited = set()
    # start at the top left corner
    start = (1, 0)
    # add the start to the heap
    heappush(heap, (0, start))
    # while the heap is not empty
    while heap:
        # pop the next item off the heap
        current, pos = heappop(heap)
        # if the current item is the end, return the distance
        if pos[1] == len(data) - 1:
            print (-current)
            continue
            
        # if the current item has not been visited
        if pos not in visited:
            # add the current item to visited
            visited.add(pos)
            # for each direction
            if data[pos[1]][pos[0]] in steep_map:
                valid_directions = steep_map[data[pos[1]][pos[0]]]
            else:
                valid_directions = directions

            for direction in valid_directions:
                # calculate the new position
                new_pos = (pos[0] + direction[0], pos[1] + direction[1])
                # if the new position is in bounds
                if 0 <= new_pos[0] < len(data[0]) and 0 <= new_pos[1] < len(data):
                    # if the new position is not a wall
                    if data[new_pos[1]][new_pos[0]] != '#':
                        # add the new position to the heap with a distance of the current distance + 1
                        heappush(heap, (current - 1, new_pos))

print (part1(read_data()))
