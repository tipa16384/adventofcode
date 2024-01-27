from math import prod

def read_data():
    # create a set called rocks that contains the (x,y) coordinates of all '#' in the input
    rocks = set()
    start = set()
    for y, line in enumerate(open('puzzle21.dat')):
        for x, char in enumerate(line.strip()):
            if char == '#':
                rocks.add((x, y))
            elif char == 'S':
                start.add((x, y))
    # reopen the input and figure out the dimensions of the grid
    width = len(line.strip())
    height = y + 1
    return set(start), rocks, (width, height)

def next_generation(alive: set, rocks: set, size: tuple) -> set:
    # return a new set of all points that are orthogonally adjacent to a point in alive but not in the set of rocks
    new_alive = set()
    for x, y in alive:
        for dx, dy in ((0, 1), (0, -1), (1, 0), (-1, 0)):
            if (((x + dx) % size[0], (y + dy) % size[1])) not in rocks:
                new_alive.add((x + dx, y + dy))
    return new_alive

def part1():
    alive, rocks, size = read_data()
    for _ in range(64):
        alive = next_generation(alive, rocks, size)
    return len(alive)

print ("Part 1:", part1())

def part2a():
    alive, rocks, size = read_data()
    key_x = [1, 129, 260, 391, 522, 653, 784]

    for gen in range(1, max(key_x)+1):
        alive = next_generation(alive, rocks, size)
        if gen in key_x:
            print (gen, len(alive))
                    
    return len(alive)

def part2():
    alive = set()
    alive.add((0,0))
    increment_count = 7577
    count = increment_count
    max_steps = 26501365
    steps = 128
    steps_increment = 131
    gen = 0
    
    while True:
        number_filled_plots = (2 * gen * (gen + 1) + 1)
        steps = steps_increment * number_filled_plots - 3
        print (gen, steps, increment_count * number_filled_plots)
        if steps > max_steps:
            break
        gen += 1

key_points = [(1, 4), (129, 14922), (260, 60189), (391, 135802), (522, 241761), (653, 378066), (784, 544717)]
# , (784, 544717)

def poly(x: int) -> int:
    term = []
    for i in range(len(key_points)):
        num, den = 1,1
        for j in range(len(key_points)):
            if i == j:
                continue
            num *= x - key_points[j][0]
            den *= key_points[i][0] - key_points[j][0]
        term.append((num * key_points[i][1]) / den)
        print (term)
    return int(sum(term))

print ("Poly:", poly(26501365))



# part2a()
# 7577 -- 7596
# starter = 128 7577
# right 1 = 259 7577
# up 1 = 259 7577
# down 1 = 259 7577
# up/right 1 = 390 7577
# period appears to be 131 after the first
# up 2 = 390 7577
# up 3 = 521 7577

#          #

#          #
#         ###
#          #

#          #
#         ###
#        #####
#         ###
#          #

# 1, 5, 13 -- sums of two consecutive squares (2*n*(n+1)+1)

