with open("puzzle6.txt") as f:
    puzzle = f.read()

def seek_unique(puzzle, packet_size=4):
    for i in range(len(puzzle)):
        if len(set(puzzle[i:i+packet_size])) == packet_size:
            return i+packet_size

print ("Part 1:", seek_unique(puzzle))
print ("Part 2:", seek_unique(puzzle, 14))
