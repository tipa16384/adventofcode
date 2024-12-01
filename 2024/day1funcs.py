from collections import Counter

def sum_diffs(left, right):
    return sum(abs(l - r) for l, r in zip(left, right))

def sum_similar(left, right):
    similarity_dict = Counter(right)
    similarity = sum(l * similarity_dict[l] for l in left)
    return similarity

def day1_data(file):
    data = [line.split() for line in file]
    left = sorted(int(l) for l, _ in data)
    right = sorted(int(r) for _, r in data)
    return left, right
