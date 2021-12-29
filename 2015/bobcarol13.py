import re
from collections import defaultdict
from itertools import permutations

def read_data():
    seats = defaultdict(lambda: defaultdict(int))
    with open('bobcarol13.dat') as f:
        for line in f.read().split('\n'):
            m = re.match(
                r'^(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\.$', line)
            seats[m.group(1)][m.group(4)] = int(m.group(3)) * \
                (-1 if m.group(2) == 'lose' else 1)
    return seats

def score(seats, order):
    return sum(seats[person][order[(i+1) % len(order)]] + seats[person][order[i-1]]
        for i, person in enumerate(order))

seats = read_data()

print("Part 1:", max(score(seats, x) for x in permutations(seats.keys())))

seats['You'] = defaultdict(int)

print("Part 2:", max(score(seats, x) for x in permutations(seats.keys())))
