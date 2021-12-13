from statistics import median
import numpy as np

delims = '()[]{}<>'
corrupt = [0, 3, 57, 1197, 25137]

def read_input():
    with open('puzzle10.dat', 'r') as f:
        for line in f:
            yield line.strip()

def score_line(line):
    score = 0
    for c in line:
        pos = delims.find(c)
        val = pos // 2 + 1
        if pos % 2 == 1:
            if val != score % 5:
                return corrupt[val], 0
            score //= 5
        else:
            score = score * 5 + val

    return 0, int(np.base_repr(score, 5)[::-1], 5)

# Part 1
print(sum(score_line(line)[0] for line in read_input()))

# Part 2
print(median(score_line(line)[1]
            for line in read_input() if score_line(line)[1] > 0))
