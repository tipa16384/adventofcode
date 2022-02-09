from functools import reduce

compass = [[0, 1], [1, 0], [0, -1], [-1, 0]]

def new_heading(heading, move):
    return (heading + 1 if move[0] == 'R' else heading - 1) % 4

def travel(heading, arr):
    return [new_heading(heading, arr[0])] * int(arr[0][1:]) + travel(new_heading(heading, arr[0]), arr[1:]) if arr else []

def journey(loc, moves):
    return [loc] + (journey((loc[0] + moves[0][0], loc[1] + moves[0][1]), moves[1:]) if moves else [])

def look_for_duplicates(path):
    return path[0] if path[0] in path[1:] else look_for_duplicates(path[1:])

with open('1.dat') as f:
    moves = f.read().strip().split(', ')

head_seq = travel(0, moves)

path = journey((0, 0), list(zip(
    list(compass[h][0] for h in head_seq), list(compass[h][1] for h in head_seq))))

print("Part 1:", abs(path[-1][0]) + abs(path[-1][1]))

dupe = look_for_duplicates(path)

print("Part 2:", abs(dupe[0]) + abs(dupe[1]))
