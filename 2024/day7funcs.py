import time
import concurrent.futures
from multiprocessing import Pool

def day7_data(file) -> list:
    lines = file.read().decode('utf-8').splitlines()
    return [
        (int(result), list(map(int, operands.split())))
        for line in lines
        for result, operands in [line.split(': ')]
    ]

def insert_operands(result, operands, operators, value):
    if value > result: return False
    if not operands: return value == result
    for op in operators:
        new_value = value
        match op:
            case '+':
                new_value += operands[0]
            case '*':
                new_value *= operands[0]
            case '||':
                new_value = new_value * (10 ** len(str(operands[0]))) + operands[0]
        if insert_operands(result, operands[1:], operators, new_value):
            return True
    return False

global_operators = []

def worker(line) -> int:
    result, operands, operators = line
    return result if insert_operands(result, operands[1:], operators, operands[0]) else 0

def parser(data: list, operators: list) -> int:
    data_with_operators = [(result, operands, operators) for result, operands in data]
    start = time.time()
    # with Pool() as pool:
    #     result = sum(pool.map(worker, data_with_operators))
    result = sum(map(worker, data_with_operators))
    print(f'Parser: {time.time() - start}')
    return result
