from Box import Box
import re

def read_rules():
    rules = list()
    with open("puzzle22.dat", "r") as f:
        for line in f:
            m = re.match(
                r"^(on|off) x=([\d-]+)\.\.([\d-]+),y=([\d-]+)\.\.([\d-]+),z=([\d-]+)\.\.([\d-]+)$", line)
            _state, x1, x2, y1, y2, z1, z2 = m.groups()
            state = _state == 'on'
            rules.append((state, int(x1), int(x2), int(
                y1), int(y2), int(z1), int(z2)))
    return rules

def diff_boxes(box_list, rule):
    new_box_list = list()
    rule_box = Box(rule[1], rule[3], rule[5], rule[2], rule[4], rule[6])
    for box in box_list:
        new_box_list.extend(box - rule_box)
    return new_box_list

def count_lights(rules, bounds=None):
    box_list = list()
    for rule_num, rule in enumerate(rules):
        if rule[0]:
            nbox = Box(rule[1], rule[3], rule[5], rule[2], rule[4], rule[6])
            if bounds: nbox &= bounds
            if nbox:
                rule_box_list = [nbox]
                for sub_rule in rules[rule_num+1:]:
                    rule_box_list = diff_boxes(rule_box_list, sub_rule)
                box_list.extend(rule_box_list)
    return sum(box.area() for box in box_list)

print("Part 1:", count_lights(read_rules(), Box(-50,-50,-50,50,50,50)))
print("Part 2:", count_lights(read_rules()))
