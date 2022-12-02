package com.chasingdings.adventofcode.y2022;

import java.io.IOException;

public class Advent {
    
    public static void main(String[] args) {
        if (args.length < 1) {
            System.err.println("Expected DAY1, DAY2 etc.");
            System.exit(-1);
        }

        PuzzleType puzzleType = PuzzleType.valueOf(args[0]);
        if (puzzleType == null) {
            System.err.println("Expected a puzzle code (DAY1, DAY2, etc)");
            System.exit(-2);
        }

        var puzzle = PuzzleFactory.makePuzzle(puzzleType);

        System.out.println(String.format("Now running %s", puzzle.getPuzzleName()));

        try {
            System.out.println("Reading in puzzle data...");
            var content = puzzle.readData(puzzle.getDataFilePath());
            System.out.println("Solving part 1...");
            var part1Solution = puzzle.solve1(content).toString();
            System.out.println("Solving part 2...");
            var part2Solution = puzzle.solve2(content).toString();
            System.out.println("Part 1: " + part1Solution);
            System.out.println("Part 2: " + part2Solution);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
