def snafu_to_decimal(s):
    value, power, queue = 0, 1, list(s)
    while queue:
        ch = queue.pop()
        match ch:
            case '0'|'1'|'2': value += int(ch) * power
            case '-': value -= power
            case '=': value -= 2*power
        power *= 5

    return value

def decimal_to_snafu(d):
    value = ''
    while d:
        d, r = divmod(d, 5)
        match r:
            case 0|1|2: value = str(r) + value
            case 3:
                d += 1
                value = '=' + value
            case 4:
                d += 1
                value = '-' + value
    return value

with open(r'2022\puzzle25.txt', 'r') as f:
    print ("Part 1:", decimal_to_snafu(sum(map(snafu_to_decimal, f.read().splitlines()))))
