#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdbool.h>
#include <assert.h>

// Function to parse a null-terminated string into an array of integers
// The first element of the array is the count of numbers that follow
int* parse_integers(const char* input) {
    static int result[21]; // Maximum size is 20 numbers + 1 for the count
    int count = 0;

    char buffer[1024];
    strncpy(buffer, input, sizeof(buffer) - 1);
    buffer[sizeof(buffer) - 1] = '\0'; // Ensure null termination

    char* token = strtok(buffer, " ");
    while (token != NULL && count < 20) {
        result[count + 1] = atoi(token);
        count++;
        token = strtok(NULL, " ");
    }

    result[0] = count; // Store the count as the first element
    return result;
}

// Recursive function to determine if the target can be achieved
bool evaluate_recursive(int *numbers, int size, long long target, long long current) {
    if (size == 0) {
        return current == target;
    }

    // Early termination for large values
    if (current > target && size > 0 && numbers[0] >= 0) {
        return false;
    }

    // Take the next number from the list
    int next_number = numbers[0];
    int *remaining = numbers + 1;

    // Try addition
    if (evaluate_recursive(remaining, size - 1, target, current + next_number)) {
        return true;
    }

    // Try multiplication
    if (evaluate_recursive(remaining, size - 1, target, current * next_number)) {
        return true;
    }

    // Try concatenation
    if (current != 0) {
        char current_str[21], next_number_str[21];
        snprintf(current_str, sizeof(current_str), "%lld", current);
        snprintf(next_number_str, sizeof(next_number_str), "%d", next_number);

        char concatenated_str[42];
        snprintf(concatenated_str, sizeof(concatenated_str), "%s%s", current_str, next_number_str);

        long long concatenated = atoll(concatenated_str);
        if (evaluate_recursive(remaining, size - 1, target, concatenated)) {
            return true;
        }
    }

    return false;
}

// Process a single line to determine if it matches the target
long long process_line(char *line) {
    char *target_str = strtok(line, ": ");
    char *numbers_str = target_str + strlen(target_str) + 2;

    long long target = atoll(target_str);
    int *parsed_numbers = parse_integers(numbers_str);

    int count = parsed_numbers[0];
    int *numbers = &parsed_numbers[1];

    return evaluate_recursive(numbers, count, target, 0) ? target : 0;
}

// Calculate the total calibration result
long long calculate_total_calibration(char **lines, int line_count) {
    long long total = 0;
    for (int i = 0; i < line_count; i++) {
        total += process_line(lines[i]);
    }
    return total;
}

// Unit tests for parse_integers
void test_parse_integers() {
    // Test case: empty string
    int *result = parse_integers("");
    assert(result[0] == 0);

    // Test case: one integer
    result = parse_integers("42");
    assert(result[0] == 1);
    assert(result[1] == 42);

    // Test case: ten integers
    result = parse_integers("1 2 3 4 5 6 7 8 9 10");
    assert(result[0] == 10);
    for (int i = 1; i <= 10; i++) {
        assert(result[i] == i);
    }

    // Test case: nineteen integers
    result = parse_integers("1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19");
    assert(result[0] == 19);
    for (int i = 1; i <= 19; i++) {
        assert(result[i] == i);
    }

    printf("All tests passed!\n");
}

int main() {
    test_parse_integers();

    // Read input from file
    FILE *file = fopen("in.txt", "r");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    char *lines[1000];
    int line_count = 0;
    char buffer[1024];

    while (fgets(buffer, sizeof(buffer), file)) {
        lines[line_count] = strdup(buffer);
        line_count++;
    }
    fclose(file);

    // Measure execution time
    clock_t start_time = clock();
    long long total_calibration = calculate_total_calibration(lines, line_count);
    clock_t end_time = clock();

    printf("Total Calibration Result: %lld\n", total_calibration);
    printf("Execution Time: %.6f seconds\n", (double)(end_time - start_time) / CLOCKS_PER_SEC);

    // Free allocated memory
    for (int i = 0; i < line_count; i++) {
        free(lines[i]);
    }

    return 0;
}
