package com.chasingdings.y2022;

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
            // calculate elapsed time for next statement
            var start = System.nanoTime();
            var part1Solution = puzzle.solve1(content).toString();
            var end = System.nanoTime();
            var elapsed = (end - start) / 1000000;
            System.out.println(String.format("Part 1 solution: %s (took %d ms)", part1Solution, elapsed));
            System.out.println("Solving part 2...");
            start = System.nanoTime();
            var part2Solution = puzzle.solve2(content).toString();
            end = System.nanoTime();
            elapsed = (end - start) / 1000000;
            System.out.println(String.format("Part 2 solution: %s (took %d ms)", part2Solution, elapsed));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
