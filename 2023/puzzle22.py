import re
import random

def read_data():
    with open("puzzle22.dat") as f:
        for line in f.read().splitlines():
            coords = re.findall(r'\d+', line)
            a, b = tuple(map(int, coords[:3])), tuple(map(int, coords[3:]))
            a1 = (min(a[0], b[0]), min(a[1], b[1]), min(a[2], b[2]))
            b1 = (max(a[0], b[0])+1, max(a[1], b[1])+1, max(a[2], b[2])+1)
            hsl_color = (random.randint(0, 360), 1, 0.5)
            # convert to hsl_color to rgb
            red = 0
            green = 0
            blue = 0
            h = hsl_color[0]
            s = hsl_color[1]
            l = hsl_color[2]
            c = (1 - abs(2*l - 1)) * s
            x = c * (1 - abs((h/60) % 2 - 1))
            m = l - c/2
            if 0 <= h < 60:
                red = c
                green = x
                blue = 0
            elif 60 <= h < 120:
                red = x
                green = c
                blue = 0
            elif 120 <= h < 180:
                red = 0
                green = c
                blue = x
            elif 180 <= h < 240:
                red = 0
                green = x
                blue = c
            elif 240 <= h < 300:
                red = x
                green = 0
                blue = c
            elif 300 <= h < 360:
                red = c
                green = 0
                blue = x
            red = int((red + m) * 255)
            green = int((green + m) * 255)
            blue = int((blue + m) * 255)
            color = (red, green, blue)
            
            yield a1, b1, color

def part1(falling: list):
    safe_to_destroy = len(falling) - 1
    
    # for each piece in falling after the first, call drop with the list of falling excluding that piece and subtract
    # 1 from safe_to_destroy if drop returns True
    for i in range(1, len(falling)):
        if drop(falling[:i] + falling[i+1:], True):
            safe_to_destroy -= 1
    
    print ("Part 1: It is safe to destroy {} pieces".format(safe_to_destroy))

def part2(falling: list):
    
    number_obliterated = 0
    
    # for each piece in falling after the first, call drop with the list of falling excluding that piece and subtract
    # 1 from safe_to_destroy if drop returns True
    for i in range(1, len(falling)):
        number_obliterated += drop(falling[:i] + falling[i+1:])
    
    print ("Part 2: {} pieces could be obliterated".format(number_obliterated))

def common(parts_is_parts):
    falling = [((-100,-100,-1), (100,100,0), (20,20,20))] + list(read_data())
    # sort falling by the third element of the first tuple
    falling.sort(key=lambda x: x[0][2])

    drop(falling)

    parts_is_parts(falling)

def drop(falling: list, scanning_for_drop: bool = False) -> int:
    drop_count = 0
    for i, piece in enumerate(falling):
        if i == 0: continue
        maxz = -100000
        for j in range(i):
            lower_piece = falling[j]
            if overlap((lower_piece[0][0], lower_piece[1][0]), (piece[0][0], piece[1][0])) and \
                overlap((lower_piece[0][1], lower_piece[1][1]), (piece[0][1], piece[1][1])) and \
                    lower_piece[1][2] > maxz:
                maxz = lower_piece[1][2]
        deltaz = piece[0][2] - maxz
        if deltaz > 0:
            if scanning_for_drop:
                return True
            falling[i] = ((piece[0][0], piece[0][1], piece[0][2] - deltaz), (piece[1][0], piece[1][1], piece[1][2] - deltaz), piece[2])
            drop_count += 1
            
    return drop_count

def overlap(a, b):
    if a == b: return True
    # a and b are two ranges
    # return true if they overlap
    return a[0] < b[1] and b[0] < a[1]

common(part1)
common(part2)

