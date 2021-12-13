from os import read


def read_data():
    "read a list of integers from puzzle1a.dat"
    data = []
    with open('puzzle1.dat') as f:
        for line in f:
            data.append(int(line))
    return data

def find_best(data):
    "find the product of the two integers in list that sum to 2020"
    for i in range(len(data)):
        for j in range(len(data)):
            if data[i] + data[j] == 2020:
                return data[i] * data[j]

def find_bester(data):
    "find the product of the three integers in data that sum to 2020"
    for i in range(len(data)):
        for j in range(len(data)):
            for k in range(len(data)):
                if data[i] + data[j] + data[k] == 2020:
                    return data[i] * data[j] * data[k]

data = read_data()
print(find_best(data))
print(find_bester(data))

