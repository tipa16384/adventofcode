# okay let's-a go

with open("puzzle1.dat", "r") as f:
    data = [sum(int(x) for x in line.split()) for line in f.read().split("\n\n")]

print ("Part 1:", max(data))
print ("Part 2:", sum(sorted(data, reverse=True)[:3]))
