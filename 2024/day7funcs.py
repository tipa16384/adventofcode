import time
import concurrent.futures

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

def worker(result, operands, operators: list) -> int:
    return result if insert_operands(result, operands[1:], operators, operands[0]) else 0

def parser(data: list, operators: list) -> int:
    start = time.time()
    with concurrent.futures.ProcessPoolExecutor() as executor:
        results = [executor.submit(worker, result, operands, operators) for result, operands in data]
        result = sum(result.result() for result in results)
    print(f'Parser: {time.time() - start}')
    return result
