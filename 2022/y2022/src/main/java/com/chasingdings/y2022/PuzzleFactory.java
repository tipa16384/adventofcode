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
            case DAY5:
                puzzle = new Puzzle5();
                break;
            case DAY6:
                puzzle = new Puzzle6();
                break;
            case DAY7:
                puzzle = new Puzzle7();
                break;
            case DAY8:
                puzzle = new Puzzle8();
                break;
        }

        return puzzle;
    }
}
