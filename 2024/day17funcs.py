import re
import random

def day17data(file) -> list:
    data = file.read().decode('utf-8').split('\n\n')
    # list 'registers' are the three numbers from the three lines in data[0]. Ignore non-numeric characters
    registers = [int(re.sub(r'\D', '', line)) for line in data[0].splitlines()]
    # instructions is a list of all the distinct numbers found in data[1]
    instructions = [int(num) for num in re.findall(r'\d+', data[1])]
    print (registers, instructions)
    return registers, instructions

def combo_value(operand: int, registers: list) -> int:
    assert operand >= 0 and operand < 7
    if operand < 4:
        return operand
    return registers[operand - 4]

def part1(registers: list, instructions: list) -> str:
    pc = 0

    output = []

    while pc < len(instructions):
        instruction = instructions[pc]
        operand = instructions[pc + 1]
        pc += 2
    
        match instruction:
            case 0: # adv
                denominator = pow(2, combo_value(operand, registers))
                registers[0] //= denominator
            case 1: # bxl
                registers[1] ^= operand
            case 2: # bst
                registers[1] = combo_value(operand, registers) % 8
            case 3: # jnz
                if registers[0] != 0:
                    pc = operand
            case 4: # bxc
                registers[1] ^= registers[2]
            case 5: # out
                output.append(combo_value(operand, registers) % 8)
            case 6: # bdv
                denominator = pow(2, combo_value(operand, registers))
                registers[1] = registers[0] // denominator
            case 7: #cdv
                denominator = pow(2, combo_value(operand, registers))
                registers[2] = registers[0] // denominator

    return ",".join(map(str, output))

def monte_solve(registers: list, instructions: list, low: int, high: int, output_len: int, end_match: str) -> tuple:
    monte = {}

    print (low, high, output_len, end_match)

    num_monte = 0

    while num_monte == 0:
        for _ in range(10000):
            target = random.randint(low, high)
            registers[0] = target
            output = part1(registers, instructions)
            #print (output, end_match)
            if output_len == len(output) and output[-len(end_match):] == end_match:
                print (end_match, output)
                monte[target] = True
                num_monte += 1
            else:
                monte[target] = False
    
    high_success = max(x for x in monte if monte[x])
    low_success = min(x for x in monte if monte[x])
    # high_monte is lowest monte > high_success
    high_monte = min([key for key in monte if key > high_success], default=high_success)
    # low_monte is highest monte < low_success
    low_monte = max([key for key in monte if key < low_success], default=low_success)
    print (high_success, low_success, high_monte, low_monte)
    return high_monte, low_monte

def part2(registers: list, instructions: list) -> int:
    target_program = ','.join(map(str, instructions))

    low = 0
    high = 0
    a = 1

    while True:
        registers[0] = pow(2,a)
        output = part1(registers, instructions)
        print (a, target_program, output)
        if len(output) < len(target_program):
            low = pow(2,a)
        if len(output) > len(target_program):
            high = pow(2,a)
            break
        a += 1

    print (low, high)

    for i in range(1, min(21, len(target_program)), 2):
        high, low = monte_solve(registers, instructions, low, high, len(target_program), target_program[-i:])
        registers[0] = (low + high) // 2
        output = part1(registers, instructions)
        print (low, high, target_program, output)

    print ("Time for brute force")

    for i in range(low, high + 1):
        registers[0] = i
        output = part1(registers, instructions)
        if output == target_program:
            print (i)
            return i

    return 0
