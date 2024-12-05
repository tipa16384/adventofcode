from collections import defaultdict
from functools import cmp_to_key

def day5_data(file) -> tuple:
    data = file.read().decode('utf-8')
    ordering_lines, page_lines = data.split('\n\n')
    
    page_ordering_map = defaultdict(set)
    for line in ordering_lines.splitlines():
        prior_page, page = map(int, line.split('|'))
        page_ordering_map[page].add(prior_page)
    
    updates = [list(map(int, line.split(','))) for line in page_lines.splitlines()]
    return page_ordering_map, updates

def druckbar(page_ordering_map: dict, pages: list) -> bool:
    seen_pages = set()
    for page in reversed(pages):
        if page_ordering_map[page].intersection(seen_pages):
            return False
        seen_pages.add(page)
    return True

def page_ordering(page_ordering_map: dict, page_tuple_1: tuple, page_tuple_2: tuple) -> int:
    if page_tuple_2[1] in page_ordering_map[page_tuple_1[1]]:
        return 1
    elif page_tuple_1[1] in page_ordering_map[page_tuple_2[1]]:
        return -1
    else:
        return page_tuple_1[0] - page_tuple_2[0]

def test_druckbar(page_ordering_map: dict, pages: list) -> list:
    compare = lambda x, y: page_ordering(page_ordering_map, x, y)
    indexed_pages = list(enumerate(pages))
    sorted_pages = sorted(indexed_pages, key=cmp_to_key(compare))
    return [page for _, page in sorted_pages]

def alles_in_Ordnung(page_ordering_map: dict, updates: list) -> int:
    return sum(update[len(update) // 2] for update in updates if druckbar(page_ordering_map, update))

def mach_es_richtig(page_ordering_map: dict, updates: list) -> int:
    return sum(
        test_druckbar(page_ordering_map, update)[len(update) // 2]
        for update in updates
        if not druckbar(page_ordering_map, update)
    )