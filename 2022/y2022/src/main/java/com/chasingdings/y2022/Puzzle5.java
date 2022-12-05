package com.chasingdings.y2022;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Puzzle5 extends AbstractPuzzle {
    private static final String DATA_FILE = "2022\\puzzle5.txt";

    @Override
    public Object solve1(String content) {
        return process(content, false);
    }

    @Override
    public Object solve2(String content) {
        return process(content, true);
    }

    @Override
    public String getDataFilePath() {
        return DATA_FILE;
    }

    @Override
    public String getPuzzleName() {
        return "Day 5 - Supply Stacks";
    }

    private void fillStack(String line, List<String> crates) {
        IntStream.range(0, 9).forEach(i -> {
            Character c = line.charAt(i * 4 + 1);
            if (c != ' ') {
                crates.set(i, crates.get(i) + c);
            }
        });
    }

    private List<String> readCrates(List<String> inputData) {
        var crates = Arrays.asList("", "", "", "", "", "", "", "", "");

        IntStream.range(0, 8)
                .mapToObj(i -> inputData.get(7 - i))
                .forEach(line -> fillStack(line, crates));

        return crates;
    }

    private void moveCrates(String line, List<String> crates, boolean is9001) {
        var tokens = line.split("\\s+");
        var num = Integer.parseInt(tokens[1]);
        var from = Integer.parseInt(tokens[3]) - 1;
        var to = Integer.parseInt(tokens[5]) - 1;

        var toMove = crates.get(from).substring(crates.get(from).length() - num);
        crates.set(from, crates.get(from).substring(0, crates.get(from).length() - num));
        
        // if not is9001, reverse toMove
        if (!is9001) {
            toMove = new StringBuilder(toMove).reverse().toString();
        }
        
        crates.set(to, crates.get(to) + toMove);
    }

    private String process(String content, boolean is9001) {
        var inputData = getInputDataByLine(content);
        var crates = readCrates(inputData);

        // inputData = sublist of inputData from 10 to end
        inputData.subList(10, inputData.size())
                .forEach(line -> moveCrates(line, crates, is9001));

        // answer is the concatenation of the last character of all the crates
        return crates.stream()
                .map(s -> s.substring(s.length() - 1))
                .collect(Collectors.joining());
    }
}
