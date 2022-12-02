package aoc.y2022;

import java.io.IOException;

public interface Puzzle {
    public static final String EOL = "\\r\\n";

    void solve() throws IOException;
    String readData(String fileName) throws IOException;
}
