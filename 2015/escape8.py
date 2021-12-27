def parse(s: str) -> int:
    len, state = 0, 0
    for c in s:
        if state or c != '"':
            if c == '\\':
                if state == 0:
                    state = 1
                else:
                    len += 1
                    state = 0
                continue
            elif state and c == 'x':
                state = 2
            elif state == 2:
                state = 3
            else:
                len += 1
                state = 0
    return len

def encode(s: str) -> int:
    return 2 + sum(2 if c == '\\' or c == '"' else 1 for c in s)

def read_input(fn) -> None:
    with open('escape8.dat') as f:
        for l in f.read().split('\n'):
            yield fn(l)

print (f"Part 1: {sum(read_input(lambda l: len(l) - parse(l)))}")
print (f"Part 2: {sum(read_input(lambda l: encode(l) - len(l)))}")

