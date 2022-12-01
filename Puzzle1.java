package aoc.y2022;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Puzzle1 {
    private static final String DATA_FILE = "C:\\Workspaces\\jre11test\\aoc\\y2022\\puzzle1.dat";
    private static final String EOL = "\\r\\n";

    private List<Integer> readData() throws IOException {
        var content = new String(Files.readAllBytes(Paths.get(DATA_FILE)));

        return Arrays.stream(content.split(EOL + EOL))
                .map(elf -> Arrays.stream(elf.split(EOL)).mapToInt(Integer::parseInt).sum())
                .sorted(Collections.reverseOrder())
                .collect(Collectors.toList());
    }

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
