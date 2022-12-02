package com.chasingdings.adventofcode.y2022;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public abstract class AbstractPuzzle implements Puzzle {
    
    @Override
    public String readData(String fileName) throws IOException {
        return new String(Files.readAllBytes(Paths.get(fileName)));
    }
}
