import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

public class Puzzle8 {
    private static final List<Integer> NUMBERS = Arrays.asList(2, 3, 4, 7);

    private static Stream<String> wordstream(String line) {
        return Arrays.asList(line.trim().split("\\s+")).subList(11, 15).stream();
    }

    private static String[] readFile(String fileName) {
        String[] lines = null;
        try {
            lines = new String(Files.readAllBytes(Paths.get(fileName))).split("\n");
        } catch (IOException e) {
            e.printStackTrace();
        }
        return lines;
    }

    public static void main(String[] args) {
        System.out.println(String.format("Part 1: %d",
                Arrays.stream(readFile("puzzle8.dat")).flatMap(Puzzle8::wordstream).map(String::length)
                        .filter(NUMBERS::contains).count()));
    }
}
