using Distributed
using Dates

# Recursive function to determine if the target can be achieved
function evaluate_recursive(numbers::Vector{Int}, target::Int, current::Int=0)
    if isempty(numbers)
        return current == target
    end

    # Early termination for large values
    if current > target && all(x -> x >= 0, numbers)
        return false
    end

    # Take the next number from the list
    next_number = numbers[1]
    remaining = numbers[2:end]

    # Try addition
    if evaluate_recursive(remaining, target, current + next_number)
        return true
    end

    # Try multiplication
    if evaluate_recursive(remaining, target, current * next_number)
        return true
    end

    # Try concatenation (if current is not zero to avoid leading zeros in concatenation)
    if current != 0 && evaluate_recursive(remaining, target, parse(Int, string(current, next_number)))
        return true
    end

    return false
end

# Process a single line to determine if it matches the target
function process_line(line::String)
    parts = split(line, ": ")
    target = parse(Int, parts[1])
    numbers = parse.(Int, split(parts[2]))
    return evaluate_recursive(numbers, target) ? target : 0
end

# Calculate the total calibration result
function calculate_total_calibration(input_lines::Vector{String})
    results = map(process_line, input_lines)
    return sum(results)
end

# Main execution block
function main()
    # Read input from file
    input_lines = readlines("in.txt")

    # Measure execution time
    start_time = now()
    total_calibration = calculate_total_calibration(input_lines)
    end_time = now()

    println("Total Calibration Result: $total_calibration")
    println("Execution Time: $(end_time - start_time)")
end

# Ensure main runs only when executed directly
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
