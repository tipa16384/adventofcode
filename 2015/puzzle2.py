def read_puzzle():
    "read puzzle2.dat as a list of lines of integers separated by 'x'"
    with open('puzzle2.dat', 'r') as f:
        for line in f:
            yield [int(x) for x in line.strip().split('x')]

paper_size = 0

for x, y, z in read_puzzle():
    paper_size += 2 * x * y + 2 * x * z + 2 * y * z + min(x * y, x * z, y * z)
print('Part 1:', paper_size)

ribbon_size = 0

for x, y, z in read_puzzle():
    ribbon_size += min(2*x+2*y, 2*x+2*z, 2*y+2*z) + x * y * z
print('Part 2:', ribbon_size)
