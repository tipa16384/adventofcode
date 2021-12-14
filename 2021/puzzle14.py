from collections import defaultdict

def read_input():
    with open('puzzle14.dat', 'r') as f:
        lines = f.readlines()
        template = lines[0].strip()
        subs = dict(yield_pairs(lines[2:]))
    return template, subs

def yield_pairs(lines):
    for line in lines:
        line = line.strip()
        toks = line.split(' -> ')
        yield toks[0], toks[1]

def salt_dicts(template):
    letter_counts = defaultdict(lambda: 0)
    pair_counts = defaultdict(lambda: 0)

    for c in template:
        letter_counts[c] += 1
    for i in range(len(template)):
        pair = template[i:i+2]
        if len(pair) == 2:
            pair_counts[pair] += 1

    return letter_counts, pair_counts

def polymerize(letter_counts, pair_counts, steps):
    for i in range(steps):
        ndict = defaultdict(lambda: 0)
        for k, v in pair_counts.items():
            if k in subs:
                ndict[k[0] + subs[k]] += v
                ndict[subs[k] + k[1]] += v
                letter_counts[subs[k]] += v
        pair_counts = ndict
    letter_tuples = [(k, v) for k, v in letter_counts.items()]
    letter_tuples = sorted(letter_tuples, key=lambda x: x[1], reverse=True)
    print (letter_tuples[0][1] - letter_tuples[-1][1])

template, subs = read_input()
letter_counts, pair_counts = salt_dicts(template)
polymerize(letter_counts, pair_counts, 10)
letter_counts, pair_counts = salt_dicts(template)
polymerize(letter_counts, pair_counts, 40)

