from functools import lru_cache
from re import match, compile

def read_data() -> tuple:
    with open("puzzle12.dat") as f:
        lines = f.read().splitlines()
    return [(z.split()[0], eval('('+z.split()[1]+')')) for z in lines]

@lru_cache
def getMatchLength(i):
    return compile(r"[\#\?]{%i}(\.|\?|$)" % i)

@lru_cache
def calc_groups(s: str, groups: tuple) -> int:
    # print ('calc_groups', s, groups)
    if not s: return 0 if groups else 1
    if not groups: return 0 if '#' in s else 1
    # if first char is '.', call calc_groups with the string without the leading '.'
    m = match(r"\.+", s)
    if m: return calc_groups(s[m.span()[1]:], groups)
    m = match(getMatchLength(groups[0]), s)
    matches = 0
    # if we have a match, call calc_groups with the string after the match and the groups[1:]
    if m: matches += calc_groups(s[m.span()[1]:], groups[1:])
    # if first char is '?' call calc_groups with the string without the leading '?'
    if s[0] == '?': matches += calc_groups(s[1:], groups)
    return matches

f = lambda x: sum([calc_groups(d[0], d[1]) for d in x])

data = read_data()
print ("Part 1:", f(data))
print ("Part 2:", f([('?'.join([d[0]]*5), d[1] * 5) for d in data]))
