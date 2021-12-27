from functools import reduce
with open('puzzle1a.dat') as f: data = list(map(int, f.read().split('\n')))
print(reduce(lambda x, y: x + 1 if y > x else x, data[1:], data[0]))