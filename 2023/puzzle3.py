import re
from itertools import chain

def part1(data, width, height, numberList):
    numbers = [n for n in numberList \
            if any([isSymbol(data[srow][scol]) for srow, scol in getAdjacent(height, width, n)])]
    print ("Part 1:", sum(x[3] for x in numbers))

def part2(data, width, height, numberList):
    # make a list of coordinates of every '*' in the data
    stars = [(row, col) for row, line in enumerate(data) for col, ch in enumerate(line) if ch == '*']
    numberDict = {n: getAdjacent(height, width, n) for n in numberList}
    starDict = {star: [n for n, v in numberDict.items() if star in v] for star in stars}
    print ("Part 2:", sum([x[0][3] * x[1][3] for x in [v for v in starDict.values() if len(v) == 2]]))

# extractNumbers: take a string, extract all the numbers from it, returning a list of tuples containing row,
# column, length, and the number itself as a string
def extractNumbers(line, row):
    return [(row, x[1], len(x[0]), int(x[0])) \
            for x in zip(re.findall(r'\d+', line), [x.start() for x in re.finditer(r'\d+', line)])]

# given a width, a height, and a tuple from extractNumbers, return a list of coordinates adjacent to the number
# string
def getAdjacent(height, width, number):
    adjacentCoords = [(number[0] - 1, number[1] -1 + x) for x in range(number[2]+2)] + \
                    [(number[0] + 1, number[1] -1 + x) for x in range(number[2]+2)] + \
                    [(number[0], number[1] - 1), (number[0], number[1] + number[2])]
    return [x for x in adjacentCoords if x[0] >= 0 and x[0] < height and x[1] >= 0 and x[1] < width]

# return True if the character is not a digit and is not a period
def isSymbol(ch): return not ch.isdigit() and ch != '.'

# readData: read the data file into a list of lists stripping newlines
def readData():
    with open("puzzle3.dat") as f:
        return f.read().splitlines()

if __name__ == "__main__":
    data = readData()
    width, height = len(data[0]), len(data)
    numberList = list(chain(*[extractNumbers(line, row) for row, line in enumerate(data)]))
    part1(data, width, height, numberList)
    part2(data, width, height, numberList)
