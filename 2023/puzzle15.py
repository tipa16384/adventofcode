import re
from functools import reduce

def deer_hash(s: str) -> int:
    return reduce(lambda h, c: (h + ord(c)) * 17 % 256, s, 0)

def read_data() -> str:
    with open('puzzle15.dat') as f:
        return f.read().strip()

def part1():
    print ("Part 1:", sum(deer_hash(x) for x in read_data().split(',')))

def part2():
    boxes = [{} for _ in range(256)]
    for x in read_data().split(','):
        label = re.match(r'^[a-z]+', x).group(0)
        box = boxes[deer_hash(label)]
        if '=' in x: box[label] = int(x.split('=')[1])
        else: box.pop(label, None)
    print ("Part 2:", sum((k+1)*(i+1)*p 
                        for k, v in enumerate(boxes) 
                        for i, p in enumerate(v.values())))

part1()
part2()
