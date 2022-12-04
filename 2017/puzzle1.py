# Written by AI

with open('puzzle1.txt', 'r') as f:
    puzzle_input = f.read()

sum_total = 0

l = len(puzzle_input)
for i in range(l):
    if i == (l - 1) and puzzle_input[i] == puzzle_input[0]:
        sum_total += int(puzzle_input[i])
    elif puzzle_input[i] == puzzle_input[(i+1) % l]:
        sum_total += int(puzzle_input[i])

print('The sum total is:', sum_total)

def solveCaptcha(captcha):
    # Initialize a variable to store the sum
    sum = 0
    
    # Get the length of the captcha
    length = len(captcha)
    
    # Iterate through the captcha string
    for i in range(length):
        # Get the current digit
        currDigit = captcha[i]
        
        # Get the next digit that is half of the length away
        nextDigit = captcha[(i + (length//2)) % length]
        
        # If they are equal, add the digit to the sum
        if currDigit == nextDigit:
            sum += int(currDigit)
    
    # Return the sum
    return sum

print (solveCaptcha(puzzle_input))
