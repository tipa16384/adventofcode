def overlap(s, f):
    a, b = s.split(',')
    a1, a2 = map(int, a.split('-'))
    b1, b2 = map(int, b.split('-'))

    # check if the ranges overlap
    return f(a1 <= b1 <= a2, a1 <= b2 <= a2) or f(b1 <= a1 <= b2, b1 <= a2 <= b2)

with open("puzzle4.dat") as f:
    data = f.read().splitlines()

print ("Part 1: ", len([x for x in data if overlap(x, lambda a,b: a and b)]))
print ("Part 2: ", len([x for x in data if overlap(x, lambda a,b: a or b)]))
