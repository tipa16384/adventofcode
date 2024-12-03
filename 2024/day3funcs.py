import re

mul_pattern = r'mul\((\d+),(\d+)\)'
do_mul_pattern = r'(do\(\)|don\'t\(\)|mul\((\d+),(\d+)\))'

def day3_data(file: str) -> str:
    return file.read().decode("utf-8")

def scan_and_multiply(text: str) -> int:
    matches = re.findall(mul_pattern, text)
    return sum(int(x) * int(y) for x, y in matches)

def scan_and_maybe_multiply(text: str) -> int:
    matches = re.findall(do_mul_pattern, text)
    total_sum = 0
    enable = True
    for match in matches:
        if match[0] == 'do()':
            enable = True
        elif match[0] == 'don\'t()':
            enable = False
        elif enable:
            total_sum += int(match[1]) * int(match[2])
    return total_sum