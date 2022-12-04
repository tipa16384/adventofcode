package com.chasingdings.y2022;

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
            case DAY3:
                puzzle = new Puzzle3();
                break;
            case DAY4:
                puzzle = new Puzzle4();
                break;
        }

        return puzzle;
    }
}
