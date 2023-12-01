from math import prod

# function to read "puzzle3.dat" as a list of string, stripping spaces and newlines
def read_file():
    with open("puzzle3.dat", "r") as f:
        return [line.strip() for line in f.readlines()]

def trees(r_init, d_init, m):
    r, d, w, t = r_init, d_init, len(m[0]), 0

    while d < len(m):
        t += m[d][r % w] == "#"
        r += r_init
        d += d_init

    return t


m = read_file()
print(trees(3, 1, m))
print(prod(trees(*init, m)
           for init in [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]))