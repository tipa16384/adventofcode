from functools import cmp_to_key


def read_part1_input():
    with open(r'2022\puzzle13.txt', 'r') as f:
        for pairs in f.read().split('\n\n'):
            pair = pairs.splitlines()
            yield (eval(pair[0]), eval(pair[1]))


def read_part2_input():
    with open(r'2022\puzzle13.txt', 'r') as f:
        return [eval(x) for x in f.read().splitlines() if x != '']


def compare_list(left, right):
    if len(left) == 0 and len(right) == 0:
        return 0
    elif len(left) == 0:
        return -1
    elif len(right) == 0:
        return 1
    else:
        cmp = compare(left[0], right[0])
        return cmp if cmp != 0 else compare(left[1:], right[1:])


def compare(left, right):
    lt = type(left)
    rt = type(right)

    if lt == int and rt == int:
        return (left > right) - (left < right)

    return compare_list(left if lt == list else [left], right if rt == list else [right])


index, score = 0, 0
for x in read_part1_input():
    index += 1
    if compare(*x) < 0:
        score += index

print("Part 1:", score)

sort_me_please = sorted(read_part2_input() +
                        [[[2]]] + [[[6]]], key=cmp_to_key(compare))

print("Part 2:", (sort_me_please.index(
    [[2]]) + 1) * (sort_me_please.index([[6]]) + 1))
