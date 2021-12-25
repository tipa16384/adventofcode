from collections import defaultdict

alu = {
    'add': lambda a, b: eval(a+"+"+b),
    'mul': lambda a, b: eval(a+"*"+b),
    'div': lambda a, b: eval(a+"//"+b),
    'mod': lambda a, b: eval(a+"%"+b),
    'eql': lambda a, b: eval("1 if "+a+" == "+b+" else 0")
}

def to_state(s: str): return f"state['{s}']" if s in 'wxyz' else s

def put_var(var: str, val: int) -> None: state[var] = val

def read_program() -> list:
    with open('monad24.dat') as f:
        return f.read().split('\n')

def run_program(program: list, inz: str = None) -> dict:
    global state
    state = defaultdict(int)
    inp_gen = (c for c in inz) if inz else None
    for l in program:
        ins = l.split()
        if ins[0] == 'inp':
            if inp_gen:
                put_var(ins[1], int(next(inp_gen)))
            else:
                put_var(ins[1], int(input("MONAD DEMANDS INPUT: ")))
        else:
            put_var(ins[1], alu[ins[0]](to_state(ins[1]), to_state(ins[2])))

    return state

run_program(read_program(), '89959794919939')
assert state['z'] == 0

run_program(read_program(), '17115131916112')
assert state['z'] == 0

run_program(read_program())

    # rules:
    # 1. I1 = I14-1   [8, 1]
    # 2. I2 = I13+6   [9, 7]
    # 3. I3 = I12     [9, 1]
    # 4. I4 = I5-4    [5, 1]
    # 5. I5 = I4+4    [9, 5]
    # 6. I6 = I7-2    [7, 1]
    # 7. I7 = I6+2    [9, 3]
    # 8. I8 = I11-5   [4, 1]
    # 9. I9 = 9       [9, 9]
    # 10. I10 = 1     [1, 1]
    # 11. I11 = I8+5  [9, 6]
    # 12. I12 = I3    [9, 1]
    # 13. I13 = I2-6  [3, 1]
    # 14. I14 = I1+1  [9, 2]
