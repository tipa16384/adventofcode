from functools import reduce
from itertools import groupby

safe_letters = 'abcdefghjkmnpqrstuvwxyz'

def increment_password(password):
    new_pwd_val = reduce(lambda x, y: x*23 + y,
                            (safe_letters.index(c) for c in password)) + 1
    return ''.join(safe_letters[(new_pwd_val // (23**i)) % 23] for i in range(8))[::-1]

def has_straight(password):
    for i in range(6):
        if ord(password[i]) == ord(password[i+1]) - 1 == ord(password[i+2]) - 2:
            return True
    return False

def has_doubles(password):
    return len(set(x for x, y in groupby(password) if len(list(y)) >= 2)) >= 2

def get_new_password(password):
    while True:
        password = increment_password(password)
        if has_doubles(password) and has_straight(password):
            return password

password = get_new_password("hxbxwxba")
print(f"Part 1: {password}")
print(f"Part 2: {get_new_password(password)}")
