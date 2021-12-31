import re

def read_data():
    aunts = {}
    with open('puzzle16.dat', 'r') as f:
        pattern = re.compile(r'(?P<name>.+?): (?P<properties>.*)')
        for line in f.read().split('\n'):
            m = pattern.match(line)
            aunts[m.group('name')] = {'enabled': True}
            for prop in m.group('properties').split(', '):
                k, v = prop.split(': ')
                aunts[m.group('name')][k] = int(v)
    return aunts

ticker = {"children": 3, "cats": 7, "samoyeds": 2, "pomeranians": 3, "akitas": 0,
          "vizslas": 0, "goldfish": 5, "trees": 3, "cars": 2, "perfumes": 1}

def sieve_aunts(which_part, match_fn):
    aunts = read_data()
    for k, v in ticker.items():
        for aunt in aunts:
            if aunts[aunt]['enabled'] and k in aunts[aunt] and not match_fn(aunts[aunt], k, v):
                aunts[aunt]['enabled'] = False
    for aunt in aunts:
        if aunts[aunt]['enabled']:
            print(f'Part {which_part}: {aunt}')

def part2_match(aunt, k, v):
    match k:
        case 'cats' | 'trees': return aunt[k] > v
        case 'pomeranians' | 'goldfish': return aunt[k] < v
        case _: return aunt[k] == v

sieve_aunts(1, lambda aunt, k, v: aunt[k] == v)
sieve_aunts(2, part2_match)
