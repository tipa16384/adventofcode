test_data = 'puzzle1a.dat'


def get_data():
    # read test_data as an array of integers
    with open(test_data) as f:
        data = [int(x) for x in f.read().splitlines()]
    return data


# calculate the total number of values that are greater than the previous one
def puzzle1(data):
    total = 0
    for i in range(1, len(data)):
        if data[i] > data[i-1]:
            total += 1
    return total


def find_depth(data, index):
    assert data and index >= 0 and index < len(data)-2
    return sum(data[index:index+3])


def puzzle1a(data):
    total = 0
    for i in range(1, len(data)-2):
        if find_depth(data, i) > find_depth(data, i-1):
            total += 1
    return total


if __name__ == '__main__':
    data = get_data()
    print(puzzle1(data))
    print(puzzle1a(data))
