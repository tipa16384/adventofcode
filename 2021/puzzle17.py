import re
from timeit import default_timer as timer

#data = "target area: x=352..377, y=-49..-30"
data = "target area: x=56..76, y=-162..-134"

def fire(v1, v2, mindv, maxdv, dvf, fvt):
    max_x = -10000
    for dv in range(mindv, maxdv):
        dv1, x = dv, 0
        for t in range(1000):
            max_x = max(max_x, x)
            if x >= v1 and x <= v2:
                yield dv, max_x, t
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

start = timer()
m = re.search(r"x=([\d-]+)\.\.([\d-]+), y=([\d-]+)\.\.([\d-]+)", data)
x1, x2, y1, y2 = int(m.group(1)), int(
    m.group(2)), int(m.group(3)), int(m.group(4))

def dxf(dx): return 0 if dx == 0 else dx - abs(dx)//dx
def dyf(dy): return dy-1

possible_xs = [xy for xy in fire(x1, x2, 1, x2+1, dxf, lambda x, _, v2: x > v2)]
possible_ys = [yy for yy in fire(y1, y2, y1-1, abs(y1)+1, dyf, lambda y, v1, _: y < v1)]
xy_times = set(t for _, _, t in possible_xs).intersection(set(t for _, _, t in possible_ys))

print('Part 1:', max(y for _, y, t in possible_ys if t in xy_times))

print('Part 2:', sum(1 for dx in range(min(possible_xs)[0], max(possible_xs)[
    0]+1) for dy in range(min(possible_ys)[0], max(possible_ys)[0]+1) if trace(dx, dy)))

print ('Time:', timer() - start)
