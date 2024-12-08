use std::fs::File;
use std::io::{self, BufRead};
use std::time::Instant;

fn evaluate_recursive(numbers: &[i64], target: i64, current: i64) -> bool {
    if numbers.is_empty() {
        return current == target;
    }

    let next_number = numbers[0];
    let remaining = &numbers[1..];

    // Early termination
    if current > target && numbers.iter().all(|&n| n >= 0) {
        return false;
    }

    // Try addition
    if evaluate_recursive(remaining, target, current + next_number) {
        return true;
    }

    // Try multiplication
    if evaluate_recursive(remaining, target, current * next_number) {
        return true;
    }

    // Try concatenation
    if current != 0 {
        let concatenated = format!("{}{}", current, next_number)
            .parse::<i64>()
            .unwrap_or(0); // Handle potential overflow
        if evaluate_recursive(remaining, target, concatenated) {
            return true;
        }
    }

    false
}

fn process_line(line: &str) -> i64 {
    let parts: Vec<&str> = line.split(": ").collect();
    let target = parts[0].parse::<i64>().unwrap();
    let numbers: Vec<i64> = parts[1].split_whitespace().map(|x| x.parse().unwrap()).collect();

    if evaluate_recursive(&numbers, target, 0) {
        target
    } else {
        0
    }
}

fn calculate_total_calibration(input_lines: Vec<String>) -> i64 {
    input_lines
        .into_iter()
        .map(|line| process_line(&line))
        .sum()
}

fn main() {
    let start_time = Instant::now();
    let input_file = File::open("in.txt").expect("Failed to open file");
    let reader = io::BufReader::new(input_file);
    let input_lines: Vec<String> = reader.lines().map(|line| line.unwrap()).collect();

    let total_calibration = calculate_total_calibration(input_lines);
    let duration = start_time.elapsed();

    println!("Total Calibration Result: {}", total_calibration);
    println!("Execution Time: {:?}", duration);
}
