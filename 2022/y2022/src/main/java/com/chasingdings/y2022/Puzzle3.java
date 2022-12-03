package com.chasingdings.y2022;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class Puzzle3 extends AbstractPuzzle {
    private static final String DATA_FILE = "2022\\puzzle3.dat";

    @Override
    public Object solve1(String content) {
        var rucksacks = getInputDataByLine(content);

        return rucksacks.stream().mapToInt(ruck -> {
            var len = ruck.length()/2;
            var left = getSet(ruck.substring(0, len));
        
            left.retainAll(getSet(ruck.substring(len)));
            return left.iterator().next();
        }).sum();
    }
    
    @Override
    public Object solve2(String content) {
        var elfGroups = groupElves(getInputDataByLine(content));

        return elfGroups.stream().mapToInt(eg -> {
            var elf = getSet(eg.get(0));
            elf.retainAll(getSet(eg.get(1)));
            elf.retainAll(getSet(eg.get(2)));
            return elf.iterator().next();
        }).sum();
    }

    @Override
    public String getDataFilePath() {
        return DATA_FILE;
    }

    @Override
    public String getPuzzleName() {
        return "Day 3 - Rusksack Reorganization";
    }

    /**
     * Take a string, map each character to its priority, and return them as a Set.
     */
    private Set<Integer> getSet(String s) {
        return s.chars().mapToObj(this::priority).collect(Collectors.toSet());
    }

    /**
     * Take a character and if it is a lower case character, return 1..26. 
     * If it is an upper case character, return 27..52.
     */
    private int priority(int c) {
        return c >= 'a' ? c - 'a' + 1 : c - 'A' + 27;
    }

    private List<List<String>> groupElves(List<String> elves) {
        // select elves in threes
        var groups = elves.stream().collect(Collectors.groupingBy(s -> elves.indexOf(s) / 3));
        return groups.values().stream().collect(Collectors.toList());
    }
}
