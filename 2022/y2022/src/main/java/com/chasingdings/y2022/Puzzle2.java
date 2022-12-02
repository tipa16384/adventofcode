package com.chasingdings.y2022;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Puzzle2 extends AbstractPuzzle {
    private static final String DATA_FILE = "2022\\puzzle2.dat";

    private final static Map<String, Integer> part1 = new HashMap<>();
    private final static Map<String, Integer> part2 = new HashMap<>();

    static {
        part1.put("A X", 4);
        part1.put("A Y", 8);
        part1.put("A Z", 3);
        part1.put("B X", 1);
        part1.put("B Y", 5);
        part1.put("B Z", 9);
        part1.put("C X", 7);
        part1.put("C Y", 2);
        part1.put("C Z", 6);

        part2.put("A X", 3);
        part2.put("A Y", 4);
        part2.put("A Z", 8);
        part2.put("B X", 1);
        part2.put("B Y", 5);
        part2.put("B Z", 9);
        part2.put("C X", 2);
        part2.put("C Y", 6);
        part2.put("C Z", 7);
    }

    @Override
    public Object solve1(String content) {
        return solveWithMap(content, part1);
    }
    
    @Override
    public Object solve2(String content) {
        return solveWithMap(content, part2);
    }

    private int solveWithMap(String content, Map<String, Integer> partMap) {
        return getInputData(content).stream().mapToInt(x -> partMap.get(x)).sum();
    }

    @Override
    public String getDataFilePath() {
        return DATA_FILE;
    }

    @Override
    public String getPuzzleName() {
        return "Day 2 - Rock Paper Scissors";
    }

    private List<String> getInputData(String content) {
        return Arrays.asList(content.split(EOL));
    }
}
