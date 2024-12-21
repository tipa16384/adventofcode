from collections import defaultdict
from functools import cache

global_pattern_dict = None

def day19data(file):
    global global_pattern_dict
    pattern_data, design_data = file.read().decode("utf-8").split('\n\n')
    global_pattern_dict = defaultdict(list)
    for pattern in pattern_data.split(', '):
        global_pattern_dict[pattern[0]].append(pattern)
    design_list = design_data.split('\n')
    print (global_pattern_dict, design_list)
    return design_list

@cache
def is_possible(design):
    if not design:
        return 1
    key = design[0]
    if key not in global_pattern_dict:
        return 0
    num_arrangements = 0
    for pattern in global_pattern_dict[key]:
        if design.startswith(pattern):
            num_arrangements += is_possible(design[len(pattern):])
    return num_arrangements

def part1(design_list):
    return sum(1 for design in design_list if is_possible(design))

def part2(design_list):
    return sum(is_possible(design) for design in design_list)

