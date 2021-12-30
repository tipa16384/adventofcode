import hashlib
from timeit import default_timer as timer

secret = 'ckczppom'

key = 1

start = timer()
while True:
    h = hashlib.md5((secret + str(key)).encode()).hexdigest()
    if h[:5] == '00000':
        print(f'Part 1: {key} in {timer() - start:.3f} seconds')
        break
    key += 1

start = timer()
while True:
    h = hashlib.md5((secret + str(key)).encode()).hexdigest()
    if h[:6] == '000000':
        print(f'Part 2: {key} in {timer() - start:.3f} seconds')
        break
    key += 1

