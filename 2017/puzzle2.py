# Written by AI

def spreadsheet_checksum(lines):
    checksum = 0
    for line in lines:
        numbers = [int(num) for num in line.split()]
        checksum += max(numbers) - min(numbers)
    return checksum

with open('puzzle2.txt', 'r') as f:
    puzzle_input = f.read()

print('Part 1:', spreadsheet_checksum(puzzle_input.splitlines()))

def row_sum(list_of_lines):
    # Initialize a variable to hold the sum
    sum = 0

    # Iterate through the list of lines
    for line in list_of_lines:
        # Split the line into individual numbers
        numbers = line.split()
        # Iterate through each number
        for num in numbers:
            # Iterate through each other number
            for other_num in numbers:
                # Convert the numbers to integers
                num = int(num)
                other_num = int(other_num)
                # Check if one of the numbers evenly divides the other
                if (num % other_num == 0 or other_num % num == 0) and num != other_num:
                    # Add the result of the division operation to the sum
                    sum += (num // other_num)
    return sum

print('Part 2:', row_sum(puzzle_input.splitlines()))

