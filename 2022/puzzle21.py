from sympy import symbols, solve, simplify

def expand(s):
    if s.isnumeric() or s == 'x': return s
    toks = s.split()
    return '(' + expand(symbols_dict[toks[0]]) + ' ' + toks[1] + ' ' + expand(symbols_dict[toks[2]]) + ')'

with open(r"2022\puzzle21.txt") as f:
    symbols_dict = {l[:4]: l[6:] for l in f.read().splitlines()}

print ("Part 1:", simplify(expand(symbols_dict['root'])))

symbols_dict['humn'] = 'x'
s = symbols_dict['root'].split()
symbols_dict['root'] = s[0] + ' - ' + s[2]

print ("Part 2:", solve(expand(symbols_dict['root']), symbols('x'))[0])
