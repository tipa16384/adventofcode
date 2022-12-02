package com.chasingdings.adventofcode.y2022;

import java.io.IOException;

public interface Puzzle {
    public static final String EOL = "\\r\\n";

    String readData(String fileName) throws IOException;
    String getPuzzleName();
    String getDataFilePath();
    Object solve1(String content);
    Object solve2(String content);
}
