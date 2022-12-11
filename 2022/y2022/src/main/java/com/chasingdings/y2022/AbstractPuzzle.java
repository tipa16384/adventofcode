package com.chasingdings.y2022;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public abstract class AbstractPuzzle implements Puzzle {
    @Override
    public String readData(String fileName) throws IOException {
        return new String(Files.readAllBytes(Paths.get(fileName)));
    }

    @Override
    public Object solve1(String content) {
        return "Not Implemented";
    }

    @Override
    public Object solve2(String content) {
        return "Not Implemented";
    }
    
    List<String> getInputDataByLine(String content) {
        return Arrays.asList(content.split(EOL));
    }

    List<Long> evalList(String content) {
        return Arrays.asList(content.split(",")).stream().map(z -> Long.parseLong(z.strip())).collect(Collectors.toList());
    }

    @Override
    public void preprocess(String content) {
        // Override this method to do any preprocessing of the input data
    }
}
