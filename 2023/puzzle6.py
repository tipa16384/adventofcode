from math import prod, ceil

puzzle = [(52, 426), (94, 1374), (75, 1279), (94, 1216)]

def solve(time_distance: tuple) -> int:
    b = time_distance[0]
    d = (b**2) - (4*time_distance[1])
    return ceil((b + d**0.5) / 2) - ceil((b - d**0.5) / 2)

# def combobulate() -> tuple:
#     l = int(''.join([str(s[0]) for s in puzzle]))
#     r = int(''.join([str(s[1]) for s in puzzle]))
#     return (l, r)

print ("Part 1:", prod([solve(td) for td in puzzle]))
# print ("Part 2:", solve(combobulate()))
print ("Part 2:", solve((52947594, 426137412791216)))
