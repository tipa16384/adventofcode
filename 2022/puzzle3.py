def priority(x):
    return ord(x) - ord('a') + 1 if x >= 'a' and x <= 'z' else ord(x) - ord('A') + 27

with open("puzzle3.dat") as f:
    data = f.read().splitlines()

part1 = sum(priority(set(x[:len(x)//2]).intersection(set(x[len(x)//2:])).pop()) for x in data)
print (part1)

part2 = sum(priority(set(x[0]).intersection(set(x[1])).intersection(set(x[2])).pop()) for x in zip(*[iter(data)]*3))
print (part2)
