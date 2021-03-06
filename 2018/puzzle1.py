from collections import defaultdict

with open('puzzle1.dat') as f:
    data = list(map(int, f.readlines()))

print ('Part 1:', sum(data))

occurrences = defaultdict(int)
index = 0
running_total = 0
while True:
    running_total += data[index]
    occurrences[running_total] += 1
    if occurrences[running_total] == 2:
        print ('Part 2:', running_total)
        break
    index = (index + 1) % len(data)
