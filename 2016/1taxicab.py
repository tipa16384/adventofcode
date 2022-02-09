fn = '1.dat'

with open(fn) as f:
    moves = f.read().strip().split(', ')

compass = [[0, 1], [1, 0], [0, -1], [-1, 0]]
x, y, heading = 0, 0, 0

for move in moves:
    heading = (heading + 1 if move[0] == 'R' else heading - 1) % 4
    dx, dy = compass[heading]
    dist = int(move[1:])
    x += dx * dist
    y += dy * dist

print (f"Part 1: {abs(x) + abs(y)}")

move_set = set()

x, y, heading = 0, 0, 0

try:
    for move in moves:
        move_set.add((x, y))
        heading = (heading + 1 if move[0] == 'R' else heading - 1) % 4
        dx, dy = compass[heading]
        dist = int(move[1:])
        for _ in range(dist):
            x += dx
            y += dy
            if (x, y) in move_set:
                raise StopIteration
            move_set.add((x, y))
except:
    pass

print (f"Part 2: {abs(x) + abs(y)}")
