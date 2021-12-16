
def gen_niceness():
    with open('puzzle5.dat', 'r') as f:
        for line in f:
            forbidden = any(x in line for x in ['ab', 'cd', 'pq', 'xy'])
            vowely = sum(line.count(x) for x in 'aeiou') >= 3
            dolby = any(x + x in line for x in line)
            yield 1 if not forbidden and vowely and dolby else 0

def gen2_niceness():
    with open('puzzle5.dat', 'r') as f:
        for line in f:
            has_repeats = any(line[i:i+2] in line[i+2:] for i in range(len(line) - 2))
            has_neighbors = any(line[i] == line[i+2] for i in range(len(line) - 2))
            yield 1 if has_repeats and has_neighbors else 0

print ('Part 1:', sum(gen_niceness()))
print ('Part 2:', sum(gen2_niceness()))

