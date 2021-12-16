# read puzzle1.dat as a string.
def read_puzzle():
    with open('puzzle1.dat', 'r') as f:
        return f.read().strip()

puzzle = read_puzzle()

print ('Part 1:', puzzle.count('(') - puzzle.count(')'))

floor = 0
for x in range(len(puzzle)):
    if puzzle[x] == '(':
        floor += 1
    elif puzzle[x] == ')':
        floor -= 1
    if floor == -1:
        print('Part 2:', x + 1)
        break