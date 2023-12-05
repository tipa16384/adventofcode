import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.io.IOException;
import java.util.Arrays;
import java.util.stream.Collectors;
import java.util.Set;

public class Puzzle4 {
    private void part1() {
        var lines = readFile("2023/puzzle4.dat");

        var part1 = lines.stream().map(this::parse)
            .map(this::intersection)
            .filter(s -> !s.isEmpty())
            .mapToInt(s -> 1 << (s.size() - 1))
            .sum();

        System.out.println("Part 1: " + part1);
    }

    // function that takes a List of Sets of Integer and returns a Set of Integer
    private Set<Integer> intersection(List<Set<Integer>> sets) {
        return sets.stream().reduce((a, b) -> {
            a.retainAll(b);
            return a;
        }).get();
    }

    // function that takes a String and returns a Set of Integer
    private List<Set<Integer>> parse(String line) {
        // split line at ":" and take the second part
        var numbers = line.split(":")[1].strip();
        // split numbers at "|"
        var numbers2 = numbers.split("\\|");
        var winningNumbers = parseNumbers(numbers2[0]);
        var myNumbers = parseNumbers(numbers2[1]);
        return Arrays.asList(winningNumbers, myNumbers);
    }

    // function that takes a String of whitespace-separated numbers and returns a
    // Set of Integer
    private Set<Integer> parseNumbers(String numbers) {
        var numbersArray = numbers.strip().split("\\s+");
        return Arrays.stream(numbersArray).map(Integer::parseInt).collect(Collectors.toSet());
    }

    // function that takes a file name and returns the contents of that file as a
    // list of strings
    private List<String> readFile(String fileName) {
        List<String> lines = null;
        try {
            lines = Files.readAllLines(Paths.get(fileName));
        } catch (IOException e) {
            e.printStackTrace();
        }
        return lines;
    }

    // main
    public static void main(String[] args) {
        Puzzle4 p = new Puzzle4();
        p.part1();
    }
}