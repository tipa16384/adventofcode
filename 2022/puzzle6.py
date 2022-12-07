import time


with open("2022\\puzzle6.txt") as f:
    puzzle = f.read()

tries = 1000

def seek_unique(puzzle, packet_size=4):
    for i in range(len(puzzle)):
        if len(set(puzzle[i:i+packet_size])) == packet_size:
            return i+packet_size


# get elapsed time for calling seek_unique(puzzle) tries times

start = time.time()
for i in range(tries):
    part1 = seek_unique(puzzle)
elapsed = (time.time() - start) / tries

print ("Part 1: {} ({:0.3f} ms)".format(part1, elapsed*1000))

start = time.time()
for i in range(tries):
    part2 = seek_unique(puzzle, 14)
elapsed = (time.time() - start) / tries

print ("Part 2: {} ({:0.3f} ms)".format(part2, elapsed*1000))
