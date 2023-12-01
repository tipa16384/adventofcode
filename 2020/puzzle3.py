
slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]  # list of slopes to check

# function to read "puzzle3.dat" as a list of string, stripping spaces and newlines
def read_file():
    with open("puzzle3.dat", "r") as f:
        return [line.strip() for line in f.readlines()]

# function that takes a list of string and a slope as input and returns the number of trees encountered
def count_trees(lines, slope):
    x, y = 0, 0
    trees = 0
    while y < len(lines):
        if lines[y][x] == "#":
            trees += 1
        x = (x + slope[0]) % len(lines[0])
        y += slope[1]
    return trees

# function that takes a list of string and a list of slopes as input and returns the product of the number of trees encountered
def count_trees_product(lines, slopes):
    product = 1
    for slope in slopes:
        product *= count_trees(lines, slope)
    return product

# main
lines = read_file()
print("Part 1:", count_trees(lines, slopes[1]))
print("Part 2:", count_trees_product(lines, slopes))
