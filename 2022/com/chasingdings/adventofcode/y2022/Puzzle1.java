package com.chasingdings.adventofcode.y2022;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * Puzzle1 is a class that attempts to solve a puzzle involving elves and
 * presents.
 * The puzzle consists of two parts:
 * 
 * Part 1: Find the elf with the largest number of presents.
 * Part 2: Find the total number of presents for the three elves with the
 * largest number of presents.
 * 
 * The class reads data from a data file, which is assumed to contain elves'
 * presents, and returns them in descending order.
 * It then uses this data to calculate and display the answers to both parts of
 * the puzzle.
 */
public class Puzzle1 {
    private static final String DATA_FILE = "2022\\puzzle1.dat";
    private static final String EOL = "\\r\\n";

    /**
     * Reads a data file containing elves' presents and returns them in descending
     * order
     *
     * @return a sorted List of integers representing the presents
     * @throws IOException if an I/O error occurs reading from the data file
     */
    private List<Integer> readData() throws IOException {
        var content = new String(Files.readAllBytes(Paths.get(DATA_FILE)));

        return Arrays.stream(content.split(EOL + EOL))
                .map(elf -> Arrays.stream(elf.split(EOL)).mapToInt(Integer::parseInt).sum())
                .sorted(Collections.reverseOrder())
                .collect(Collectors.toList());
    }

    /**
     * Solves the two parts of the problem by reading the data from the input file
     * and calculating the required output.
     *
     * @throws IOException If an I/O exception occurs
     */
    private void solve() throws IOException {
        var dataList = readData();
        System.out.println(String.format("Part 1: %d", dataList.get(0)));
        var total = IntStream.range(0, 3).map(i -> dataList.get(i)).sum();
        System.out.println(String.format("Part 2: %d", total));
    }

    public static void main(String[] args) {
        var puzzle = new Puzzle1();
        try {
            puzzle.solve();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
