import statistics

def print_min(data, statfn, weight):
    dest = int(statfn(data))
    print(min(sum(weight(abs(x-dest-y)) for x in data) for y in range(2)))

with open('puzzle7.dat') as f:
    data = list(map(int, f.read().strip().split(',')))

print_min(data, statistics.median, lambda x: x)
print_min(data, statistics.mean, lambda x: x * (x + 1) // 2)
