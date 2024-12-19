import heapq

def day18data(file) -> dict:
    timeline = [tuple(map(int, line.split(','))) for line in file.read().decode('utf-8').splitlines()]
    return {point: i + 1 for i, point in enumerate(timeline)}

def what_is_n(width, old_n=None):
    return old_n if old_n else 1024 if width == 70 else 12

def part1(block_dict: dict, width, height, n=None) -> int:
    queue = [(0, (0, 0))]
    visited = set()

    n = what_is_n(width, n)

    while queue:
        steps, (x, y) = heapq.heappop(queue)
        if (x, y) == (width, height):
            return steps
        if (x, y) in visited or x < 0 or y < 0 or x > width or y > height or \
            ((x, y) in block_dict and block_dict[(x, y)] <= n):
            continue
        visited.add((x, y))
        for dx, dy in ((0, 1), (1, 0), (0, -1), (-1, 0)):
            heapq.heappush(queue, (steps + 1, (x + dx, y + dy)))
    return 0

def part2(block_dict: dict, width, height) -> int:
    low = what_is_n(width)
    high = len(block_dict)

    while low < high-1:
        n = (low + high) // 2

        if part1(block_dict, width, height, n):
            low = n
        else:
            high = n

    for (x,y), val in block_dict.items():
        if val == n+1:
            return f"{x},{y}"
