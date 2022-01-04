def read_program() -> list:
    with open('puzzle24.dat') as f:
        return f.read().split('\n')

def analyze_input(program: list) -> None:
    current_input, z_stack, ins = -1, list(), (x for x in program)
    for instruction in ins:
        if instruction.startswith('inp'): current_input += 1
        elif instruction == 'div z 1':
            for _ in range(11): xins = next(ins)
            z_stack.append((current_input, int(xins.split()[-1])))
        elif instruction == 'div z 26':
            left, xins = z_stack.pop()
            yield left, current_input, xins + int(next(ins).split()[-1])

def hack_program(part: str, program: list, digits: str) -> str:
    display = [digits[0]] * 14

    for left, right, offset in analyze_input(program):
        for c in digits:
            rc = str(int(c) + offset)
            if rc in digits:
                display[left], display[right] = c, rc
                break

    print(f"Part {part}: {''.join(display)}")

hack_program('1', read_program(), "987654321")
hack_program('2', read_program(), "123456789")
