ops = {
    'AND': lambda x, y: x & y,
    'OR': lambda x, y: x | y,
    'LSHIFT': lambda x, y: x << y,
    'RSHIFT': lambda x, y: x >> y,
    'NOT': lambda x: ~x
}

def read_data() -> dict:
    circuit = dict()

    with open("circuit7.dat") as f:
        for x, y in (l.split(' -> ') for l in f.read().split('\n')):
            circuit[y] = x

    return circuit

def expand_circuit(value: str) -> int:
    global circuit

    if isinstance(value, int):
        return value

    if value.isnumeric():
        val = int(value)
    else:
        values = value.split(' ')

        if len(values) == 1:
            val = expand_circuit(circuit[values[0]])
        elif values[0] == 'NOT':
            val = ops['NOT'](expand_circuit(values[1]))
        else:
            val = ops[values[1]](expand_circuit(values[0]),
                                 expand_circuit(values[2]))

    circuit[value] = val

    return val


circuit = read_data()
a_value = expand_circuit('a')

print(f"Part 1: {a_value}")

circuit = read_data()
circuit['b'] = a_value

print(f"Part 2: {expand_circuit('a')}")
