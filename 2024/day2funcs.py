def day2_data(file: str) -> list:
    return [[int(x) for x in line.split()] for line in file]

def all_satisfy(values: list, func) -> bool:
    return all(func(values[i], values[i + 1]) for i in range(len(values) - 1))

def is_safe(values: list) -> bool:
    return (all_satisfy(values, lambda x, y: x <= y) or \
            all_satisfy(values, lambda x, y: x >= y)) and \
            all_satisfy(values, lambda x, y: 1 <= abs(x - y) <= 3)

def is_kinda_safe(values: list) -> bool:
    return is_safe(values) or any(is_safe(values[:i] + values[i + 1:]) \
                                  for i in range(len(values)))

def count_kinda_safe(values: list) -> int:
    return sum(is_kinda_safe(v) for v in values)

def count_safe(values: list) -> int:
    return sum(is_safe(v) for v in values)