package com.chasingdings.y2022;

import java.util.stream.IntStream;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Puzzle6 extends AbstractPuzzle {
    private static final String DATA_FILE = "2022\\puzzle6.txt";

    @Override
    public Object solve1(String content) {
        return process(content, 4);
    }

    @Override
    public Object solve2(String content) {
        return process(content, 14);
    }

    @Override
    public String getDataFilePath() {
        return DATA_FILE;
    }

    @Override
    public String getPuzzleName() {
        return "Day 6 - Tuning Trouble";
    }

    private int process(String content, int packetSize) {
        return IntStream.range(packetSize, content.length()).parallel()
                .mapToObj(i -> content.substring(i - packetSize, i))
                .filter(s -> s.chars().distinct().count() == packetSize)
                .mapToInt(content::indexOf)
                .findFirst()
                .getAsInt() + packetSize;
    }
}
