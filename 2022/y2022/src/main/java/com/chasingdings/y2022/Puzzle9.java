package com.chasingdings.y2022;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Puzzle9 extends AbstractPuzzle {
    private static final String DATA_FILE = "2022\\puzzle9.txt";
    private int part1Solution = 0;
    private int part2Solution = 0;

    @Override
    public Object solve1(String content) {
        return part1Solution;
    }

    @Override
    public Object solve2(String content) {
        return part2Solution;
    }

    @Override
    public void preprocess(String content) {
        final var puzzle = getInputDataByLine(content);
        final int ropeSize = 10;

        List<Point> snake = new ArrayList<>();
        Set<Point> part2Visited = new HashSet<>();
        Set<Point> part1Visited = new HashSet<>();
        final var origin = new Point(0, 0);

        snake = IntStream.range(0, ropeSize).mapToObj(i -> origin).collect(Collectors.toList());

        part2Visited.add(snake.get(0));
        part1Visited.add(snake.get(0));

        var curPoint = origin;

        for (var command : puzzle) {
            curPoint = movePoint(curPoint, command);

            snake.set(ropeSize - 1, curPoint);

            Boolean anythingMoved;
            do {
                anythingMoved = false;

                for (int i = ropeSize - 1; i > 0; i--) {
                    var head = snake.get(i);
                    var tail = snake.get(i - 1);

                    if (Math.sqrt(Math.pow(head.x - tail.x, 2) + Math.pow(head.y - tail.y, 2)) >= 2.0) {
                        snake.set(i - 1,
                                new Point(tail.x + justOneStep(head.x, tail.x), tail.y + justOneStep(head.y, tail.y)));
                        anythingMoved = true;
                    }
                }

                part1Visited.add(snake.get(ropeSize - 2));
                part2Visited.add(snake.get(0));
            } while (anythingMoved);

            part1Solution = part1Visited.size();
            part2Solution = part2Visited.size();
        }
    }

    private Point movePoint(Point c, String cmd) {
        char d = cmd.charAt(0);
        int dt = Integer.parseInt(cmd.substring(2));
        return d == 'U' ? new Point(c.x, c.y - dt)
                : d == 'D' ? new Point(c.x, c.y + dt) : d == 'L' ? new Point(c.x - dt, c.y) : new Point(c.x + dt, c.y);
    }

    private int justOneStep(int a, int b) {
        return (a == b) ? 0 : a > b ? 1 : -1;
    }

    @Override
    public String getDataFilePath() {
        return DATA_FILE;
    }

    @Override
    public String getPuzzleName() {
        return "Day 9 - Rope Bridge";
    }

    final class Point {
        int x;
        int y;

        public Point(int x, int y) {
            this.x = x;
            this.y = y;
        }

        // add equals
        public boolean equals(Object o) {
            if (o == this)
                return true;
            if (!(o instanceof Point)) {
                return false;
            }

            Point point = (Point) o;

            return point.x == x && point.y == y;
        }

        // add hashCode
        public int hashCode() {
            int result = 17;
            result = 31 * result + x;
            result = 31 * result + y;
            return result;
        }
    }
}
