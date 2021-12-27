# puzzle the sixth day of advent of code
def solve(days):
    buckets = [fishes.count(i) for i in range(9)]
    for i in range(days): buckets[(i+7) % 9] += buckets[i % 9]
    print(sum(buckets))

with open('puzzle6.dat') as f:
    fishes = list(map(int, f.read().strip().split(',')))

solve(80)
solve(256)
