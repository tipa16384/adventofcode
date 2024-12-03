def day20162_data(file: str) -> list:
    return [line.decode("utf-8").strip() for line in file]

adjacent = {
    '1': {'R': '2', 'D': '4'},
    '2': {'L': '1', 'R': '3', 'D': '5'},
    '3': {'L': '2', 'D': '6'},
    '4': {'U': '1', 'R': '5', 'D': '7'},
    '5': {'U': '2', 'L': '4', 'R': '6', 'D': '8'},
    '6': {'U': '3', 'L': '5', 'D': '9'},
    '7': {'U': '4', 'R': '8'},
    '8': {'U': '5', 'L': '7', 'R': '9'},
    '9': {'U': '6', 'L': '8'}
}

weird_keypad = {
    '1': {'D': '3'},
    '2': {'R': '3', 'D': '6'},
    '3': {'U': '1', 'L': '2', 'R': '4', 'D': '7'},
    '4': {'L': '3', 'D': '8'},
    '5': {'R': '6'},
    '6': {'U': '2', 'L': '5', 'R': '7', 'D': 'A'},
    '7': {'U': '3', 'L': '6', 'R': '8', 'D': 'B'},
    '8': {'U': '4', 'L': '7', 'R': '9', 'D': 'C'},
    '9': {'L': '8'},
    'A': {'U': '6', 'R': 'B'},
    'B': {'U': '7', 'L': 'A', 'R': 'C', 'D': 'D'},
    'C': {'U': '8', 'L': 'B'},
    'D': {'U': 'B'}
}

def find_code(data: list, keypad: dict, start: str) -> str:
    solution = ''
    current_position = start
    for line in data:
        for c in line:
            if c in keypad[current_position]:
                current_position = keypad[current_position][c]
        solution += current_position
    return solution

def day20162_part1(data: list) -> str:
    return find_code(data, adjacent, '5')

def day20162_part2(data: list) -> str:
    return find_code(data, weird_keypad, '5')
