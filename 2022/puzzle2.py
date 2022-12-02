part1 = { 'A X': 4, 'A Y': 8, 'A Z': 3, 'B X': 1, 'B Y': 5, 'B Z': 9, 'C X': 7, 'C Y': 2, 'C Z': 6 }
part2 = { 'A X': 3, 'A Y': 4, 'A Z': 8, 'B X': 1, 'B Y': 5, 'B Z': 9, 'C X': 2, 'C Y': 6, 'C Z': 7 }

with open("puzzle2.dat") as f:
    data = f.read().splitlines()

print ("Part 1: ", sum([part1[play] for play in data]))
print ("Part 2: ", sum([part2[play] for play in data]))
