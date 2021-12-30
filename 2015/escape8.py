def encode(s: str) -> int:
    return 2 + sum(2 if c == '\\' or c == '"' else 1 for c in s)

def read_input(fn) -> None:
    with open('escape8.dat') as f:
        for l in f.read().split('\n'):
            yield fn(l)

print (f"Part 1: {sum(read_input(lambda l: len(l) - len(eval(l))))}")
print (f"Part 2: {sum(read_input(lambda l: encode(l) - len(l)))}")
