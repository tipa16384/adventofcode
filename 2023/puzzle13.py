def read_data():
    with open("puzzle13.dat") as f:
        return [tobin(puz.splitlines()) for puz in f.read().split("\n\n")]

def tobin(puz: list) -> list:
    return (len(puz[0]), [int("".join(["1" if c == "#" else "0" for c in line]), 2) for line in puz])

def score_puzzle(lp: list, f) -> int:
    l, p = lp[0], lp[1]
    x = f(p, len(p))
    if x: return x * 100
    p = [bitme(p, l-i-1) for i in range(l)]
    return f(p, l)

def score_helper(p: list, l: int) -> int:
    for i in range(1,l):
        z = zip(p[:i][::-1], p[i:])
        if all([a == b for a, b in z]):
            return i
    return None

def score_helper_2(p: list, l: int) -> int:
    for i in range(1,l):
        z = list(zip(p[:i][::-1], p[i:]))
        number_same = sum([a == b for a, b in z])
        number_diff = sum([differ(a,b) for a, b in z])
        if number_diff == 1 and number_same + 1 == len(z):
            return i
    return None

# function returns true if a and b differ by only one bit
def differ(a: int, b: int) -> bool:
    n = abs(a-b)
    return n and (n & (n - 1) == 0)

# function bitme returns a binary number made from the i'th bit of each integer in the list
def bitme(p: list, i: int) -> int:
    mask = 1 << i
    return int("".join(["1" if (n & mask) else "0" for n in p]), 2)

print ("Part 1:", sum([score_puzzle(lp, score_helper) for lp in read_data()]))
print ("Part 2:", sum([score_puzzle(lp, score_helper_2) for lp in read_data()]))
