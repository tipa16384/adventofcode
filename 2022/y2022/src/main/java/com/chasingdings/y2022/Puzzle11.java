package com.chasingdings.y2022;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

public class Puzzle11 extends AbstractPuzzle {
    private static final String DATA_FILE = "2022\\puzzle11.txt";

    @Override
    public Object solve1(String content) {
        return playGame(content, 20, 3);
    }

    @Override
    public Object solve2(String content) {
        return playGame(content, 10000, 1);
    }

    @Override
    public void preprocess(String content) {
        // TODO Auto-generated method stub
    }

    @Override
    public String getDataFilePath() {
        return DATA_FILE;
    }

    @Override
    public String getPuzzleName() {
        return "Day 11 - Monkey in the Middle";
    }

    private long playGame(String content, int rounds, int worryDivider) {
        var monkeys = Arrays.stream(content.split(EOL + EOL)).map(z -> new Monkey(z)).collect(Collectors.toList());

        for (int i = 0; i < rounds; i++) {
            for (var monkey : monkeys) {
                for (var monkeyWorries : monkey.play(worryDivider)) {
                    monkeys.get(monkeyWorries.monkeyNumber).items.add(monkeyWorries.worry);
                }
            }
        }

        var sortedMonkeys = monkeys.stream().sorted((a, b) -> (int) (a.inspectCount - b.inspectCount))
                .collect(Collectors.toList());
        return sortedMonkeys.get(sortedMonkeys.size() - 1).inspectCount
                * sortedMonkeys.get(sortedMonkeys.size() - 2).inspectCount;
    }

    class Monkey {
        private List<Long> items;
        private BiFunction<Long, Long, Long> operation;
        private long operand;
        private long test;
        private int iftrue;
        private int iffalse;
        private long inspectCount;

        public Monkey(String monkeyDo) {
            var monkeyStats = monkeyDo.split(EOL);
            this.items = evalList(monkeyStats[1].substring(18));
            if (monkeyStats[2].charAt(25) == 'o') {
                this.operation = (a, b) -> a * a;
                this.operand = 0;
            } else if (monkeyStats[2].charAt(23) == '*') {
                this.operation = (a, b) -> a * b;
                this.operand = Long.parseLong(monkeyStats[2].substring(25));
            } else {
                this.operand = Long.parseLong(monkeyStats[2].substring(25));
                this.operation = (a, b) -> a + b;
            }
            this.test = Integer.parseInt(monkeyStats[3].substring(21));
            this.iftrue = Integer.parseInt(monkeyStats[4].substring(29));
            this.iffalse = Integer.parseInt(monkeyStats[5].substring(30));
            this.inspectCount = 0;
        }

        public List<MonkeyWorries> play(int worryDivider) {
            var result = new ArrayList<MonkeyWorries>();
            while (!this.items.isEmpty()) {
                this.inspectCount++;
                var worry = this.operation.apply(this.items.remove(0), this.operand);
                if (worryDivider == 1) {
                    worry = worry % 9699690;
                } else {
                    worry = worry / worryDivider;
                }
                result.add(new MonkeyWorries(worry % this.test == 0 ? this.iftrue : this.iffalse, worry));
            }
            return result;
        }
    }

    class MonkeyWorries {
        private int monkeyNumber;
        private long worry;

        public MonkeyWorries(int monkeyNumber, long worry) {
            this.monkeyNumber = monkeyNumber;
            this.worry = worry;
        }
    }
}
