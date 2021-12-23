import itertools

class Box:
    def intersection(self, other):
        a, b = self, other
        x1 = max(min(a.x1, a.x2), min(b.x1, b.x2))
        y1 = max(min(a.y1, a.y2), min(b.y1, b.y2))
        z1 = max(min(a.z1, a.z2), min(b.z1, b.z2))
        x2 = min(max(a.x1, a.x2), max(b.x1, b.x2))
        y2 = min(max(a.y1, a.y2), max(b.y1, b.y2))
        z2 = min(max(a.z1, a.z2), max(b.z1, b.z2))
        if x1<x2 and y1<y2 and z1<z2:
            return type(self)(x1, y1, z1, x2-1, y2-1, z2-1)
    __and__ = intersection

    def area(self):
        return (self.x2-self.x1)*(self.y2-self.y1)*(self.z2-self.z1)

    def difference(self, other):
        inter = self&other
        if not inter:
            #print ("Not intersecting")
            yield self
            return
        xs = {self.x1, self.x2}
        ys = {self.y1, self.y2}
        zs = {self.z1, self.z2}
        if self.x1<other.x1<self.x2: xs.add(other.x1)
        if self.x1<other.x2<self.x2: xs.add(other.x2)
        if self.y1<other.y1<self.y2: ys.add(other.y1)
        if self.y1<other.y2<self.y2: ys.add(other.y2)
        if self.z1<other.z1<self.z2: zs.add(other.z1)
        if self.z1<other.z2<self.z2: zs.add(other.z2)
        for (x1, x2), (y1, y2), (z1, z2) in itertools.product(
            pairwise(sorted(xs)), pairwise(sorted(ys)), pairwise(sorted(zs))
        ):
            rect = type(self)(x1, y1, z1, x2-1, y2-1, z2-1)
            if rect!=inter:
                yield rect
    __sub__ = difference

    def __init__(self, x1, y1, z1, x2, y2, z2):
        if x1>x2 or y1>y2 or z1>z2:
            print (x1, y1, z1, x2, y2, z2)
            raise ValueError("Coordinates are invalid")
        self.x1, self.y1, self.z1, self.x2, self.y2, self.z2 = x1, y1, z1, x2+1, y2+1, z2+1
    
    def __iter__(self):
        yield self.x1
        yield self.y1
        yield self.z1
        yield self.x2
        yield self.y2
        yield self.z2

    def __eq__(self, other):
        return isinstance(other, Box) and tuple(self)==tuple(other)
    def __ne__(self, other):
        return not (self==other)

    def __repr__(self):
        return f"Box({self.x1}, {self.y1}, {self.z1}, {self.x2-1}, {self.y2-1}, {self.z2-1})"

def pairwise(iterable):
    # https://docs.python.org/dev/library/itertools.html#recipes
    a, b = itertools.tee(iterable)
    next(b, None)
    return zip(a, b)

