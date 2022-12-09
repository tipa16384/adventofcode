package com.chasingdings.y2022;

import java.util.ArrayList;
import java.util.List;

public class Puzzle8 extends AbstractPuzzle {
    private static final String DATA_FILE = "2022\\puzzle8.txt";

    private List<TreeInfo> grid = null;

    @Override
    public Object solve1(String content) {
        parsePuzzle(content);
        return grid.stream().mapToInt(r -> r.visible).sum();
    }

    @Override
    public Object solve2(String content) {
        parsePuzzle(content);
        return grid.stream().mapToInt(r -> r.treeCount).max().getAsInt();
    }

    private void parsePuzzle(String content) {
        if (grid != null) {
            return;
        }

        var puzzle = getInputDataByLine(content);

        grid = new ArrayList<>();
        for (int x = 0; x < puzzle.get(0).length(); x++) {
            for (int y = 0; y < puzzle.size(); y++) {
                grid.add(getResults(puzzle, x, y));
            }
        }
    }

    private TreeInfo calcView(List<String> puzzle, int startX, int startY, int dx, int dy) {
        int height = puzzle.size();
        int width = puzzle.get(0).length();
        char startTree = puzzle.get(startY).charAt(startX);
        int numTrees = 0;
        int x = startX;
        int y = startY;
        int isVisible = 0;

        while (true) {
            x += dx;
            y += dy;

            if (x < 0 || x >= width || y < 0 || y >= height) {
                isVisible = 1;
                break;
            }

            numTrees++;

            if (puzzle.get(y).charAt(x) >= startTree) {
                break;
            }
        }

        return new TreeInfo(isVisible, numTrees);
    }

    private TreeInfo getResults(List<String> puzzle, int x, int y) {
        List<TreeInfo> results = new ArrayList<>();
        results.add(calcView(puzzle, x, y, 1, 0));
        results.add(calcView(puzzle, x, y, -1, 0));
        results.add(calcView(puzzle, x, y, 0, 1));
        results.add(calcView(puzzle, x, y, 0, -1));

        int anyVisible = results.stream().mapToInt(r -> r.visible).max().getAsInt();
        int totalTrees = results.stream().mapToInt(r -> r.treeCount).reduce(1, (a, b) -> a * b);

        return new TreeInfo(anyVisible, totalTrees);
    }

    @Override
    public String getDataFilePath() {
        return DATA_FILE;
    }

    @Override
    public String getPuzzleName() {
        return "Day 8 - Tree Top Treehouse";
    }

    class TreeInfo {
        int visible;
        int treeCount;

        public TreeInfo(int visible, int treeCount) {
            this.visible = visible;
            this.treeCount = treeCount;
        }
    }
}
