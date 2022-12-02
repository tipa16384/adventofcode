package com.chasingdings.adventofcode.y2022;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Puzzle1 extends AbstractPuzzle {
    private static final String DATA_FILE = "2022\\puzzle1.dat";
    
    @Override
    public Object solve1(String content) {
        return getInputData(content).get(0);
    }
    
    @Override
    public Object solve2(String content) {
        var dataList = getInputData(content);
        return IntStream.range(0, 3).map(i -> dataList.get(i)).sum();
    }

    private List<Integer> getInputData(String content) {
        return Arrays.stream(content.split(EOL + EOL))
                .map(elf -> Arrays.stream(elf.split(EOL)).mapToInt(Integer::parseInt).sum())
                .sorted(Collections.reverseOrder())
                .collect(Collectors.toList());
    }

    @Override
    public String getDataFilePath() {
        return DATA_FILE;
    }

    @Override
    public String getPuzzleName() {
        return "Day 1 - Calorie Counting";
    }
}
