def read_program() -> list:
    with open('puzzle24.dat') as f:
        return f.read().split('\n')

def hack_program(part: str, program: list, digits: str) -> str:
    display = [digits[0]] * 14

    for left, right, offset in analyze_input(program):
        for c in digits:
            rc = int(c) + offset
            if rc in range(1, 10):
                display[left], display[right] = c, str(rc)
                break

    print(f"Part {part}: {''.join(display)}")

def analyze_input(program: list) -> None:
    current_input = -1
    z_stack = []
    ins = (x for x in program)
    for instruction in ins:
        if instruction.startswith('inp'):
            current_input += 1
        elif instruction == 'div z 1':
            for _ in range(11):
                xins = next(ins)
            xins = int(xins.split()[-1])
            z_stack.append((current_input, xins))
        elif instruction == 'div z 26':
            xdel = int(next(ins).split()[-1])
            left, xins = z_stack.pop()
            yield left, current_input, xins + xdel

hack_program('1', read_program(), "987654321")
hack_program('2', read_program(), "123456789")
