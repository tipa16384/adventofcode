import java.io.*;
import java.util.*;

public class AdventCalibration {

    // Recursive function to determine if the target can be achieved
    public static boolean evaluateRecursive(List<Long> numbers, long target, long current) {
        if (numbers.isEmpty()) {
            return current == target;
        }

        long nextNumber = numbers.get(0);
        List<Long> remaining = numbers.subList(1, numbers.size());

        // Try addition
        if (evaluateRecursive(remaining, target, current + nextNumber)) {
            return true;
        }

        // Try multiplication
        if (evaluateRecursive(remaining, target, current * nextNumber)) {
            return true;
        }

        // Try concatenation
        if (current != 0) {
            long concatenated = Long.parseLong(current + "" + nextNumber);
            if (evaluateRecursive(remaining, target, concatenated)) {
                return true;
            }
        }

        return false;
    }

    // Process a single line to determine if it matches the target
    public static long processLine(String line) {
        String[] parts = line.split(": ");
        long target = Long.parseLong(parts[0]);

        List<Long> numbers = new ArrayList<>();
        for (String numberStr : parts[1].split(" ")) {
            numbers.add(Long.parseLong(numberStr));
        }

        return evaluateRecursive(numbers, target, 0) ? target : 0;
    }

    // Calculate the total calibration result
    public static long calculateTotalCalibration(List<String> lines) {
        long total = 0;
        for (String line : lines) {
            total += processLine(line);
        }
        return total;
    }

    public static void main(String[] args) {
        // Read input from file
        List<String> lines = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader("in.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                lines.add(line);
            }
        } catch (IOException e) {
            System.err.println("Failed to read input file: " + e.getMessage());
            return;
        }

        // Measure execution time
        long startTime = System.nanoTime();
        long totalCalibration = calculateTotalCalibration(lines);
        long endTime = System.nanoTime();

        System.out.println("Total Calibration Result: " + totalCalibration);
        System.out.printf("Execution Time: %.6f seconds\n", (endTime - startTime) / 1_000_000_000.0);
    }
}
