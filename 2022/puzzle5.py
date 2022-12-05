import re

def crate_me(input_str: str, is9001: bool) -> str:
    crates = [[] for i in range(9)]

    for i in range(7, -1, -1):
        for j in range(1, 36, 4):
            crate_num = (j-1) // 4
            box = input_str[i][j]
            if box != " ":
                crates[crate_num].append(box)

    pattern = re.compile("move (\d+) from (\d+) to (\d+)")

    # for each line from 10 to the end
    for line in input_str[10:]:
        # match the pattern
        match = pattern.match(line)
        # get the number, from, and to
        num = int(match.group(1))
        frm = int(match.group(2)) - 1
        to = int(match.group(3)) - 1

        if not is9001:
            # move the number from the from crate to the to crate
            for i in range(num):
                crates[to].append(crates[frm].pop())
        else:
            tomove = crates[frm][-num:]
            crates[frm] = crates[frm][:-num]
            crates[to] = crates[to] + tomove
    
    return "".join(crates[i][-1] for i in range(9))

with open("puzzle5.txt", "r") as f:
    input_str = f.read().splitlines()

# Part 1 answer is a string of the last element in each crate
print ("Part 1:", crate_me(input_str, False))
print ("Part 2:", crate_me(input_str, True))

