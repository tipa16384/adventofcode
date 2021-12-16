import hashlib

secret = 'ckczppom'

key = 1

while True:
    h = hashlib.md5((secret + str(key)).encode()).hexdigest()
    if h[:5] == '00000':
        print('Part 1:', key)
        break
    key += 1

while True:
    h = hashlib.md5((secret + str(key)).encode()).hexdigest()
    if h[:6] == '000000':
        print('Part 2:', key)
        break
    key += 1

