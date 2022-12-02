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
            puzzle.solve();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
