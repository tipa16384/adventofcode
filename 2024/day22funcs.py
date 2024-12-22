from collections import defaultdict

def day22data(file):
    return list(map(int, file.read().decode('utf-8').splitlines()))

def part1(data: list) -> int:
    return sum(generate2000th(secret) for secret in data)

def part2(data: list) -> int:
    market_map = defaultdict(int)
    for secret in data:
        prices, sequence_to_price = price_set(secret)
        for price in prices:
            market_map[price] += sequence_to_price[price]

    best_sequence = None
    best_result = 0

    for sequence, count in market_map.items():
        result = count
        if result > best_result:
            best_result = result
            best_sequence = sequence
            print (best_sequence, best_result)
    return best_result

def generate2000th(secret: int) -> int:
    for _ in range(2000):
        secret = next_sequence(secret)
    return secret

def next_sequence(secret: int) -> int:
    secret = prune(mix(secret, secret << 6))
    secret = prune(mix(secret, secret >> 5))
    secret = prune(mix(secret, secret << 11))
    return secret

def price_set(secret: int) -> tuple:
    prices = set()
    secret_list = list()
    sequence_to_price = dict()
    for _ in range(2000):
        secret = next_sequence(secret)
        secret_list.append(secret % 10)
    for i in range(4, 2000):
        s1 = secret_list[i-3] - secret_list[i-4]
        s2 = secret_list[i-2] - secret_list[i-3]
        s3 = secret_list[i-1] - secret_list[i-2]
        s4 = secret_list[i] - secret_list[i-1]
        sequence = (s1, s2, s3, s4)
        if sequence not in prices:
            prices.add(sequence)
            sequence_to_price[sequence] = secret_list[i]
    return prices, sequence_to_price

def mix(secret: int, mixee: int) -> int:
    return secret ^ mixee

prune_mask = (1 << 24) - 1

def prune(secret: int) -> int:
    return secret & prune_mask

assert mix(42, 15) == 37
assert prune(100000000) == 16113920
