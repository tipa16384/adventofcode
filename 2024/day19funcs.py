from collections import defaultdict
from functools import cache

def day19data(file):
    pattern_data, design_data = file.read().decode("utf-8").split('\n\n')
    pattern_dict = defaultdict(list)
    for pattern in pattern_data.split(', '):
        pattern_dict[pattern[0]].append(pattern)
    design_list = design_data.split('\n')
    print (pattern_dict, design_list)
    return pattern_dict, design_list

global_pattern_dict = None

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

def part1(pattern_dict, design_list):
    global global_pattern_dict
    global_pattern_dict = pattern_dict
    return sum(1 for design in design_list if is_possible(design))

def part2(pattern_dict, design_list):
    global global_pattern_dict
    global_pattern_dict = pattern_dict
    return sum(is_possible(design) for design in design_list)

