
# function part1 takes a list of string and returns the sum of calling scoreList on each string
def part1(data): return sum([scoreList(countDuplicates(line)) for line in data])

def part2(data):
    duplicates = [countDuplicates(line) for line in data]
    part2_sum = sum(1 + play_game(duplicates[x:]) for x in range(len(duplicates)))
    return part2_sum


# score a list by returning 0 if the list has one or fewer elements, or 2 ** (n-1) if the list has n elements
def scoreList(n): return 0 if n == 0 else 2 ** (n - 1)

def play_game(numbers):
    # if the list is empty, return 0
    if not numbers:
        return 0
    # if head is 0, return 0
    if numbers[0] == 0:
        return 0
    # otherwise, call play_game with the next x elements of the list and add x
    x = numbers[0]
    return x + sum(play_game(numbers[z:]) for z in range(1, x + 1))

# Example usage
numbers = [3, 4, 0, 2, 5]
print(play_game(numbers))

# function that takes a string, splits it into tokens and then returns the number of duplicate tokens
def countDuplicates(line):
    tokens = line.split()
    return len(tokens) - len(set(tokens))

# readData: read the data file into a list of lists stripping newlines
def readData():
    with open("puzzle4.dat") as f:
        return f.read().splitlines()

if __name__ == "__main__":
    data = readData()
    print ("Part 1:", part1(data))
    print ("Part 2:", part2(data))
