package com.chasingdings.adventofcode.y2022;

public class PuzzleFactory {
    public static Puzzle makePuzzle(PuzzleType puzzleType) {
        Puzzle puzzle = null;

        switch (puzzleType) {
            case DAY1:
                puzzle = new Puzzle1();
                break;
            case DAY2:
                puzzle = new Puzzle2();
                break;
        }

        return puzzle;
    }
}
