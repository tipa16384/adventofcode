package aoc.y2022;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Puzzle2 implements Puzzle {
    private static final String DATA_FILE = "C:\\Workspaces\\jre11test\\aoc\\y2022\\puzzle2.dat";

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
    public String readData(String fileName) throws IOException {
        return new String(Files.readAllBytes(Paths.get(fileName)));
    }

    @Override
    public void solve() throws IOException {
        var dataList = getInputData();
        var part1Score = dataList.stream().mapToInt(x -> part1.get(x)).sum();
        System.out.println(String.format("Part 1: %d", part1Score));
        var part2Score = dataList.stream().mapToInt(x -> part2.get(x)).sum();
        System.out.println(String.format("Part 2: %d", part2Score));
    }

    private List<String> getInputData() throws IOException {
        var content = readData(DATA_FILE);

        return Arrays.asList(content.split(EOL));
    }

    public static void main(String[] args) {
        var puzzle = new Puzzle2();
        try {
            puzzle.solve();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
