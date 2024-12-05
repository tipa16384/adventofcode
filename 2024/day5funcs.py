from functools import cmp_to_key

def day5_data(file) -> tuple:
    data = file.read().decode('utf-8')
    ordering_lines, page_lines = data.split('\n\n')
    return [tuple(map(int, line.split('|'))) for line in ordering_lines.splitlines()], \
            [list(map(int, line.split(','))) for line in page_lines.splitlines()]

def beide_teile(page_order_list: list, updates: list) -> tuple:
    part2_sum = part1_sum = 0

    for update in updates:
        sorted_update = sorted(update, key=cmp_to_key(lambda x, y: page_ordering(page_order_list, x, y)))
        middle_element = sorted_update[len(update) // 2]
        if sorted_update == update:
            part1_sum += middle_element
        else:
            part2_sum += middle_element
    return part1_sum, part2_sum

def page_ordering(page_order_list: list, page1: int, page2: int) -> int:
    if (page1,page2) in page_order_list: return -1
    if (page2,page1) in page_order_list: return 1
    return 0
