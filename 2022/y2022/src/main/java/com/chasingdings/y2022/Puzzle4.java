package com.chasingdings.y2022;

import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class Puzzle4 extends AbstractPuzzle {
    private static final String DATA_FILE = "2022\\puzzle4.dat";
    private static final Pattern PATTERN = Pattern.compile("(\\d+)\\-(\\d+),(\\d+)\\-(\\d+)");

    @Override
    public Object solve1(String content) {
        Boolinator andMe = (a, b) -> a && b;
        return process(content, andMe);
    }

    @Override
    public Object solve2(String content) {
        Boolinator orMe = (a, b) -> a || b;
        return process(content, orMe);
    }

    @Override
    public String getDataFilePath() {
        return DATA_FILE;
    }

    @Override
    public String getPuzzleName() {
        return "Day 4 - Camp Cleanup";
    }

    interface Boolinator {
        boolean pleaseDo(boolean a, boolean b);
    }

    private int process(String content, Boolinator boolinator) {
        var assignmentList = getInputDataByLine(content);
        return assignmentList.stream()
                .filter(task -> isOverlap(task, boolinator))
                .collect(Collectors.toList())
                .size();
    }

    private boolean isOverlap(String s, Boolinator boolinator) {
        var m = PATTERN.matcher(s);
        if (m.matches()) {
            int x1 = Integer.parseInt(m.group(1));
            int x2 = Integer.parseInt(m.group(2));
            int y1 = Integer.parseInt(m.group(3));
            int y2 = Integer.parseInt(m.group(4));
            return boolinator.pleaseDo(x1 <= y1 && y1 <= x2, x1 <= y2 && y2 <= x2)
                    || boolinator.pleaseDo((y1 <= x1 && x1 <= y2), (y1 <= x2 && x2 <= y2));
        }
        return false;
    }
}
