package com.chasingdings.y2022;

import java.io.File;
import java.io.IOException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.LoggerContext;

public class Advent {
    private static final Logger logger = LogManager.getLogger(Advent.class);
    private static final String LOG4J2_CONF = "2022\\y2022\\log4j2.xml";
    
    static final int RUN_TIMES = 1000;
    
    public static void main(String[] args) {
        File file = new File(LOG4J2_CONF);
        LoggerContext context = (org.apache.logging.log4j.core.LoggerContext) LogManager.getContext(false);
        context.setConfigLocation(file.toURI());

        if (args.length < 1) {
            logger.error("Expected DAY1, DAY2 etc.");
            System.exit(-1);
        }

        PuzzleType puzzleType = PuzzleType.valueOf(args[0]);
        if (puzzleType == null) {
            logger.error("Expected a puzzle code (DAY1, DAY2, etc)");
            System.exit(-2);
        }

        var puzzle = PuzzleFactory.makePuzzle(puzzleType);

        logger.info("Now running {}}", puzzle.getPuzzleName());

        try {
            logger.info("Reading in puzzle data...");
            var content = puzzle.readData(puzzle.getDataFilePath());
            logger.info("Solving part 1...");
            // calculate elapsed time for next statement
            long elapsedTime = 0;
            var part1Solution = "None";
            for (int i = 0; i < RUN_TIMES; i++) {
                var start = System.nanoTime();
                part1Solution = puzzle.solve1(content).toString();
                var end = System.nanoTime();
                elapsedTime += end - start;
            }
            var elapsed = elapsedTime / (1000000f * RUN_TIMES);
            logger.info("Part 1 solution: {} (took {} ms)", part1Solution, elapsed);
            logger.info("Solving part 2...");
            elapsedTime = 0;
            var part2Solution = "None";
            for (int i = 0; i < RUN_TIMES; i++) {
                var start = System.nanoTime();
                part2Solution = puzzle.solve2(content).toString();
                var end = System.nanoTime();
                elapsedTime += end - start;
            }
            elapsed = elapsedTime / (1000000f * RUN_TIMES);
            logger.info("Part 2 solution: {} (took {} ms)", part2Solution, elapsed);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
