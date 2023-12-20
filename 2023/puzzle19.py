import re
    
rule_dict = { 'A': lambda _: True, 'R': lambda _: False }

def process_rule(rule_code):
    m = re.match(r'^(\w+)\{(.*)\}', rule_code)
    rule_name = m.group(1)
    rule = 'lambda part: '
    for instr in m.group(2).split(','):
        if ':' not in instr:
            rule += "rule_dict['{}'](part)".format(instr)
        else:
            cond, cons = instr.split(':')
            prop, comp, val = cond[0], cond[1], int(cond[2:])
            rule += "rule_dict['{}'](part) if part['{}'] {} {} else ".format(cons, prop, comp, val)
    rule_dict[rule_name] = eval(rule)

def part1():
    with open('puzzle19.dat') as f:
        # split on blank line
        data = f.read().split('\n\n')
    
    for rule in data[0].splitlines():
        process_rule(rule)

    parts = [{p1.split('=')[0]: int(p1.split('=')[1]) \
        for p1 in part[1:-1].split(',')} for part in data[1].splitlines()]

    part1 = sum([sum(part.values()) for part in parts if rule_dict['in'](part)])
    print ("Part 1:",part1)

def lets_go_helper(part, rules, rule):
    rv = 0

    for step in rules[rule]:
        if ':' not in step:
            rv += lets_go(part, rules, step)
            continue
        condition, next_rule = step.split(':')
        x = condition[0]
        xv = part[x]
        val = int(condition[2:])

        # Splitting case
        modified_part = part.copy()
        if condition[1] == '<':
            modified_part[x] = (xv[0], val - 1)
            part[x] = (val, xv[1])
        else: # '>'
            modified_part[x] = (val + 1, xv[1])
            part[x] = (xv[0], val)

        rv += lets_go(modified_part, rules, next_rule)
    return rv

def lets_go(part, rules, rule):
    match rule:
        case 'A':
            return (part['x'][1]-part['x'][0]+1) * (part['m'][1]-part['m'][0]+1) * \
                (part['a'][1]-part['a'][0]+1) * (part['s'][1]-part['s'][0]+1)
        case 'R':
            return 0
        case _:
            return lets_go_helper(part, rules, rule)
        
def part2():
    with open('puzzle19.dat') as f:
        # split on blank line
        data = f.read().split('\n\n')
    
    xmas = {'x': (1,4000), 'm': (1,4000), 'a': (1,4000), 's': (1,4000)}
    p2rules = {rule[:rule.index('{')] : rule[rule.index('{')+1:-1].split(',') for rule in data[0].splitlines()}
    print ("Part 2:", lets_go(xmas, p2rules, 'in'))

part1()
part2()
