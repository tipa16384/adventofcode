#!/usr/bin/python3

# solve the puzzle at http://adventofcode.com/2017/day/5

def read_input():
    with open('puzzle5.txt', 'r') as f:
        input_ints = list(map(int, f.read().splitlines()))
    return input_ints

def solve(input_ints: list, part2: bool) -> int:
    # initialize the number of steps to 0
    steps = 0

    # initialize the current position to 0
    pos = 0

    # while the current position is valid
    while 0 <= pos < len(input_ints):
        # get the current value
        val = input_ints[pos]

        # increment the current position by the current value
        pos += val

        # increment the current value by 1
        if part2 and val >= 3:
            input_ints[pos - val] -= 1
        else:
            input_ints[pos - val] += 1

        # increment the number of steps by 1
        steps += 1

    return steps

print("Part 1:", solve(read_input(), False))
print("Part 2:", solve(read_input(), True))

