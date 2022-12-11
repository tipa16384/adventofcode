package com.chasingdings.y2022;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Puzzle10 extends AbstractPuzzle {
    private static final String DATA_FILE = "2022\\puzzle10.txt";

    private List<State> programState;

    @Override
    public Object solve1(String content) {
        return IntStream.range(0, 6)
                .map(clock -> clock * 40 + 20)
                .map(clock -> stateAt(programState, clock).x * clock)
                .sum();
    }

    @Override
    public Object solve2(String content) {
        final int width = 40;
        final int height = 6;
        
        final var spritePos = IntStream.range(0, width * height)
                .mapToObj(pixel -> stateAt(programState, pixel + 1))
                .collect(Collectors.toList());
        final var screen = IntStream.range(0, width * height)
                .mapToObj(pixel -> spritePos.get(pixel).x - 1 <= (pixel % width)
                        && (pixel % width) <= spritePos.get(pixel).x + 1 ? "#" : ".")
                .collect(Collectors.joining());

                return "\n" + IntStream.range(0, height)
                .mapToObj(i -> screen.substring(i * width, (i + 1) * width))
                .collect(Collectors.joining("\n"));
    }

    private List<State> runProgram(List<String> commands) {
        List<State> programState = new ArrayList<>();
        programState.add(new State(0, 0));

        var currentState = new State(1, 1);

        for (var command : commands) {
            var toks = command.split(" ");
            var arg = toks.length == 2 ? Integer.parseInt(toks[1]) : null;

            currentState = OP_MAP.get(toks[0]).execute(currentState, arg);
            programState.add(currentState);
        }

        return programState;
    }

    private State stateAt(List<State> programState, int t) {
        for (int i = 0; i < programState.size() - 1; i++) {
            if (programState.get(i).clock <= t && t < programState.get(i + 1).clock) {
                return programState.get(i);
            }
        }

        return programState.get(programState.size() - 1);
    }

    private final Map<String, Instruction> OP_MAP = Map.of(
            "noop", new Instruction(1, (x, y) -> x),
            "addx", new Instruction(2, (x, y) -> x + y));

    class Instruction {
        int clockCycles;
        BiFunction<Integer, Integer, Integer> func;

        public Instruction(int clockCycles, BiFunction<Integer, Integer, Integer> func) {
            this.clockCycles = clockCycles;
            this.func = func;
        }

        public State execute(State state, Integer arg) {
            return new State(state.clock + clockCycles, func.apply(state.x, arg));
        }
    }

    class State {
        int clock;
        int x;

        public State(int clock, int x) {
            this.clock = clock;
            this.x = x;
        }
    }

    @Override
    public void preprocess(String content) {
        programState = runProgram(getInputDataByLine(content));
    }

    @Override
    public String getDataFilePath() {
        return DATA_FILE;
    }

    @Override
    public String getPuzzleName() {
        return "Day 10 - Cathode-Ray Tube";
    }
}
