from collections import defaultdict

def get_instructions():
    with open('puzzle6.dat', 'r') as f:
        for line in f:
            yield line.strip()

def get_coordinates(line):
    bounds = line.split(' through ')
    x1, y1 = [int(x) for x in bounds[0].split(',')]
    x2, y2 = [int(x) for x in bounds[1].split(',')]
    for x in range(x1, x2+1):
        for y in range(y1, y2+1):
            yield (x, y)

light_array = defaultdict(bool)

for line in get_instructions():
    if line.startswith('turn on'):
        for x, y in get_coordinates(line[8:]):
            light_array[(x, y)] = True
    elif line.startswith('turn off'):
        for x, y in get_coordinates(line[9:]):
            light_array[(x, y)] = False
    elif line.startswith('toggle'):
        for x, y in get_coordinates(line[7:]):
            light_array[(x, y)] = not light_array[(x, y)]

print ('Part 1:', sum(1 for x in light_array.values() if x))

light_array = defaultdict(int)

for line in get_instructions():
    if line.startswith('turn on'):
        for x, y in get_coordinates(line[8:]):
            light_array[(x, y)] += 1
    elif line.startswith('turn off'):
        for x, y in get_coordinates(line[9:]):
            light_array[(x, y)] = max(0, light_array[(x, y)] - 1)
    elif line.startswith('toggle'):
        for x, y in get_coordinates(line[7:]):
            light_array[(x, y)] += 2

print ('Part 2:', sum(light_array.values()))


