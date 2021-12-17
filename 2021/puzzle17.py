import re

data = "target area: x=56..76, y=-162..-134"

def fire(v1, v2, dvf, fvt):
    max_x = -10000
    for dv in range(-100, 300):
        dv1, x = dv, 0
        for t in range(1000):
            max_x = max(max_x, x)
            if x >= v1 and x <= v2:
                yield dv, max_x
                break
            elif fvt(x, v1, v2):
                break
            x += dv1
            dv1 = dvf(dv1)

def trace(dx, dy):
    x, y = 0, 0
    for _ in range(1000):
        if x > x2 or y < y1:
            return False
        if x >= x1 and y <= y2:
            return True
        x, y = x + dx, y + dy
        dx, dy = dxf(dx), dyf(dy)

m = re.search(r"x=([\d-]+)\.\.([\d-]+), y=([\d-]+)\.\.([\d-]+)", data)
x1, x2, y1, y2 = int(m.group(1)), int(
    m.group(2)), int(m.group(3)), int(m.group(4))

def dxf(dx): return 0 if dx == 0 else dx - abs(dx)/dx
def dyf(dy): return dy-1

possible_xs = [xy for xy in fire(x1, x2, dxf, lambda x, _, v2: x > v2)]
possible_ys = [yy for yy in fire(y1, y2, dyf, lambda y, v1, _: y < v1)]

print('Part 1:', max(possible_ys)[1])

print('Part 2:', sum(1 for dx in range(min(possible_xs)[0], max(possible_xs)[
    0]+1) for dy in range(min(possible_ys)[0], max(possible_ys)[0]+1) if trace(dx, dy)))
