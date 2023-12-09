from itertools import groupby

def play_game(translator, card_values):
    data = [(translator(token[0], card_values), int(token[1])) \
            for line in readData() \
                for token in [line.strip().split()]]
    # value is the sum of the second element of each tuple multiplied by its index in data + 1
    return sum([i * x[1] for i, x in enumerate(sorted(data), start=1)])

def translatePart1(hand: str, card_values: str) -> str:
    return score(hand_to_groups(hand), card_values, hand)

def translatePart2(hand: str, card_values: str) -> str:
    groups = hand_to_groups(hand)
    # if there is more than one group, and the first group has a 'J' in it, merge 
    # the first and second groups
    if len(groups) > 1 and 'J' in groups[0]:
        groups[0] += groups[1]
        groups.pop(1)
    # otherwise if there is more than one group and any group but the first has a 'J' in it,
    # merge it with the first group
    elif len(groups) > 1 and any('J' in group for group in groups[1:]):
        for group in groups[1:]:
            if 'J' in group:
                groups[0] += group
                groups.remove(group)
                break
    return score(groups, card_values, hand)

def hand_to_groups(hand: str) -> list:
    # sort and group the characters in the hand
    groups = [list(g) for _, g in groupby(sorted(hand))]
    # sort groups by the length of the sublists, highest first
    return sorted(groups, key=lambda x: len(x), reverse=True)

def score(groups: list, card_values: str, hand: str) -> int:
    value = str(5 + len(groups[0]) - len(groups))
    # in the hand, translate all T to A, J to B, Q to C, K to D, A to E
    return value + ''.join(["0123456789ABC"[card_values.index(card)] for card in hand])

# readData: read the data file into a list of lists stripping newlines
def readData():
    with open("puzzle7.dat") as f:
        return f.read().splitlines()

print ("Part 1:", play_game(translatePart1, "23456789TJQKA"))
print ("Part 2:", play_game(translatePart2, "J23456789TQKA"))
