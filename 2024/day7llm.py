from multiprocessing import Pool
import time

def evaluate_recursive(numbers, target, current=0):
    """Recursively determines if the target can be achieved using +, *, and ||."""
    if not numbers:
        return current == target

    # Early termination for large values
    if current > target and all(n >= 0 for n in numbers):
        return False

    # Take the next number from the list
    next_number = numbers[0]
    remaining = numbers[1:]

    # Try addition
    if evaluate_recursive(remaining, target, current + next_number):
        return True

    # Try multiplication
    if evaluate_recursive(remaining, target, current * next_number):
        return True

    # Try concatenation (if current is not zero to avoid leading zeros in concatenation)
    if current != 0 and evaluate_recursive(remaining, target, int(str(current) + str(next_number))):
        return True

    return False

def process_line(line):
    """Processes a single line to determine if it matches the target."""
    target, numbers = line.split(": ")
    target = int(target)
    numbers = list(map(int, numbers.split()))
    if evaluate_recursive(numbers, target):
        return target
    return 0

def calculate_total_calibration(input_lines):
    """Processes all input lines in parallel and calculates the total calibration result."""
    with Pool() as pool:
        results = pool.map(process_line, input_lines)
    return sum(results)

if __name__ == "__main__":
    # Read input from file
    with open('in.txt', 'r') as file:
        data = file.readlines()

    start_time = time.time()
    # Calculate the total calibration result
    total_calibration = calculate_total_calibration(data)
    print(f"Total Calibration Result: {total_calibration}")
    print(f"Execution time: {time.time() - start_time}")
