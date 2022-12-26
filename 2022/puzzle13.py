from functools import cmp_to_key

def read_part1_input():
    with open(r'2022\puzzle13.txt', 'r') as f:
        for pairs in f.read().split('\n\n'):
            yield list(map(eval, pairs.splitlines()))

def read_part2_input():
    with open(r'2022\puzzle13.txt', 'r') as f:
        return [eval(x) for x in f.read().splitlines() if x != '']

def compare_list(left, right):
    if len(left) and len(right):
        cmp = compare(left[0], right[0])
        return cmp if cmp != 0 else compare(left[1:], right[1:])
    return compare(len(left), len(right))

def compare(left, right):
    lt, rt = type(left), type(right)
    if lt == int and rt == int: return (left > right) - (left < right)
    return compare_list(left if lt == list else [left], right if rt == list else [right])

print("Part 1:", sum(index for index, x in enumerate(read_part1_input(), 1) if compare(*x) < 0))

sort_me_please = sorted(read_part2_input() + [[[2]]] + [[[6]]], key=cmp_to_key(compare))

print("Part 2:", (sort_me_please.index([[2]]) + 1) * (sort_me_please.index([[6]]) + 1))
