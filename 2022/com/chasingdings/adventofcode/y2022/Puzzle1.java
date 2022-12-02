package com.chasingdings.adventofcode.y2022;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Puzzle1 extends AbstractPuzzle {
    private static final String DATA_FILE = "2022\\puzzle1.dat";

    @Override
    public void solve() throws IOException {
        var dataList = getInputData();
        System.out.println(String.format("Part 1: %d", dataList.get(0)));
        var total = IntStream.range(0, 3).map(i -> dataList.get(i)).sum();
        System.out.println(String.format("Part 2: %d", total));
    }

    @Override
    public String getPuzzleName() {
        return "Day 1 - Calorie Counting";
    }

    private List<Integer> getInputData() throws IOException {
        var content = readData(DATA_FILE);

        return Arrays.stream(content.split(EOL + EOL))
                .map(elf -> Arrays.stream(elf.split(EOL)).mapToInt(Integer::parseInt).sum())
                .sorted(Collections.reverseOrder())
                .collect(Collectors.toList());
    }
}
