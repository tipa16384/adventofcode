def solve(moves, part2 = False):
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
                if part2 and (x, y) in move_set:
                    raise StopIteration
                move_set.add((x, y))
    except:
        pass

    return abs(x) + abs(y)

with open('1.dat') as f:
    moves = f.read().strip().split(', ')

compass = [[0, 1], [1, 0], [0, -1], [-1, 0]]

print (f"Part 1: {solve(moves)}")
print (f"Part 2: {solve(moves, True)}")
