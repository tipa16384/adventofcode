def read_data() -> tuple:
    with open('cucumber25.dat') as f:
        data = f.read().split('\n')
        width = len(data[0])
        height = len(data)
        cukes = dict()
        for y, row in enumerate(data):
            for x, c in enumerate(row):
                if c in '>v':
                    cukes[(x, y)] = c
        return width, height, cukes

def print_map(cukes: dict, width: int, height: int) -> None:
    for y in range(height):
        for x in range(width):
            c = cukes[(x, y)] if (x, y) in cukes else '.'
            print(c, end='')
        print()
    print()

def move_cukes(cukes: dict, width: int, height: int, dir: chr) -> tuple:
    new_cukes = dict()
    moved = False
    for (x, y), c in cukes.items():
        new_pos = ((x+1) % width, y) if c == '>' else (x,(y+1) % height)
        if (c == dir) and (not new_pos in cukes):
            new_cukes[new_pos] = c
            moved = True
        else:
            new_cukes[(x, y)] = c
    assert len(new_cukes) == len(cukes)
    return new_cukes, moved

width, height, cukes = read_data()
print (width, height, len(cukes))

print_map(cukes, width, height)

turns = 0
while True:
    cukes, moved = move_cukes(cukes, width, height, '>')
    cukes, also_moved = move_cukes(cukes, width, height, 'v')
    turns += 1
    if (turns % 10) == 0:
        print (turns, len(cukes))

    if not (moved or also_moved):
        print ('Part 1: Stopped after {} turns'.format(turns))
        break
