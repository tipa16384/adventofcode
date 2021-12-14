from collections import Counter

def polymerize(template, subs, steps):
    letter_counts = Counter(template)
    pair_counts = Counter(template[i:i+2] for i in range(len(template) - 1))
    for i in range(steps):
        ndict = Counter()
        for k, v in pair_counts.items():
            if k in subs:
                ndict.update({k[0] + subs[k]: v,  subs[k] + k[1]: v})
                letter_counts.update({subs[k]: v})
        pair_counts = ndict
    print(letter_counts.most_common()[0]
        [1] - letter_counts.most_common()[-1][1])

with open('puzzle14.dat', 'r') as f:
    lines = f.readlines()
    template = lines[0].strip()
    subs = dict(line.strip().split(' -> ') for line in lines[2:])

polymerize(template, subs, 10)
polymerize(template, subs, 40)
