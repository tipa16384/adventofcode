from functools import lru_cache

def day11data(file):
    data = list(map(int, file.read().decode('utf-8').split()))
    print (data)
    return data

@lru_cache(maxsize=None)
def blink(value, num_blinks):
    if not num_blinks:
        return 1
    if not value:
        return blink(1, num_blinks - 1)
    strval = str(value)
    if len(strval) % 2 == 0:
        left = int(strval[:len(strval) // 2])
        right = int(strval[len(strval) // 2:])
        return blink(left, num_blinks - 1) + blink(right, num_blinks - 1)
    return blink(value * 2024, num_blinks - 1)

def day11parts(data, num_blinks):
    return sum(blink(x, num_blinks) for x in data)
