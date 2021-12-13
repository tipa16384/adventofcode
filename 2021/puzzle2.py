fn = 'puzzle2.dat'

def read_course():
    # open fn and read each line as a list of tuples separated by ' '
    with open(fn) as f:
        for line in f:
            yield tuple(line.split(' '))

distance = 0
depth = 0
aim = 0

for course, delta in read_course():
    if course == 'forward':
        distance += int(delta)
        depth += aim * int(delta)
    elif course == 'up':
        aim -= int(delta)
    elif course == 'down':
        aim += int(delta)
    else:
        assert False

# print the product of distance and depth
print(distance * depth)
