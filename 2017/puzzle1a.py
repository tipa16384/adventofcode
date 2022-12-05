# Get the input string
with open('puzzle1.txt', 'r') as f:
    input_str = f.read()

# Initialize the sum to 0
sum = 0

# Iterate through the digits in the input string
for i in range(len(input_str)):
    # Get the current digit and the next digit
    # Be careful to handle the case where the next digit is the first digit
    cur_digit = int(input_str[i])
    next_digit = int(input_str[(i + 1) % len(input_str)])

    # If the current digit and the next digit are equal, add the current digit to the sum
    if cur_digit == next_digit:
        sum += cur_digit

# Print the sum
print("Part 1:",sum)
