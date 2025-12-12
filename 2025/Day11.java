import java.nio.file.*;
import java.io.IOException;
import java.util.*;
import java.util.regex.*;

public class Day11 {
    private Map<String, List<String>> tree;
    private Map<Integer, Integer> memo;

    /**
     * Count the number of directed paths from start → end in a DAG.
     *
     * @param graph adjacency list: node -> list of neighbors
     * @param start starting node name
     * @param end   ending node name
     * @return number of distinct paths (long, may overflow)
     */
    public static long countPaths(
            Map<String, List<String>> graph,
            String start,
            String end) {
        // 1. Collect all nodes (keys + all destinations)
        Set<String> nodes = new HashSet<>();
        nodes.addAll(graph.keySet());
        for (List<String> targets : graph.values()) {
            nodes.addAll(targets);
        }

        // 2. Compute in-degrees for Kahn's topological sort
        Map<String, Integer> indegree = new HashMap<>();
        for (String node : nodes) {
            indegree.put(node, 0);
        }

        for (String u : graph.keySet()) {
            for (String v : graph.get(u)) {
                indegree.put(v, indegree.get(v) + 1);
            }
        }

        // 3. Kahn's algorithm
        Deque<String> queue = new ArrayDeque<>();
        for (String node : nodes) {
            if (indegree.get(node) == 0) {
                queue.add(node);
            }
        }

        List<String> topo = new ArrayList<>(nodes.size());
        while (!queue.isEmpty()) {
            String u = queue.remove();
            topo.add(u);

            List<String> edges = graph.get(u);
            if (edges != null) {
                for (String v : edges) {
                    int deg = indegree.get(v) - 1;
                    indegree.put(v, deg);
                    if (deg == 0) {
                        queue.add(v);
                    }
                }
            }
        }

        // Optional DAG validation
        if (topo.size() != nodes.size()) {
            throw new IllegalArgumentException("Graph contains a cycle; not a DAG.");
        }

        // 4. DP: ways[u] = number of paths u -> end
        Map<String, Long> ways = new HashMap<>();
        for (String n : nodes) {
            ways.put(n, 0L);
        }
        ways.put(end, 1L);

        // 5. Reverse topo order DP
        for (int i = topo.size() - 1; i >= 0; i--) {
            String u = topo.get(i);
            if (u.equals(end))
                continue;

            long sum = 0L;
            List<String> edges = graph.get(u);
            if (edges != null) {
                for (String v : edges) {
                    sum += ways.get(v);
                }
            }
            ways.put(u, sum);
        }

        return ways.getOrDefault(start, 0L);
    }

    private void part2() {
        var lines = readFile("puzzle11.dat");
        Pattern patter = Pattern.compile("\\w+");
        Map<String, List<String>> graph = new HashMap<>();

        for (String line : lines) {
            Matcher matcher = patter.matcher(line);
            var firstMatch = true;
            String key = null;
            List<String> values = new ArrayList<>();
            while (matcher.find()) {
                var match = matcher.group();
                if (firstMatch) {
                    key = match;
                    firstMatch = false;
                } else {
                    values.add(match);
                }
            }
            if (key != null && values.size() > 0) {
                graph.put(key, values);
            }
        }

        var p1 = countPaths(graph, "you", "out");

        System.out.println("Part 1: " + p1);

        long p2a = countPaths(graph, "svr", "fft");
        long p2b = countPaths(graph, "fft", "dac");
        long p2c = countPaths(graph, "dac", "out");
        System.out.println("Part 2a: " + (p2a * p2b * p2c));

        p2a = countPaths(graph, "svr", "dac");
        p2b = countPaths(graph, "dac", "fft");
        p2c = countPaths(graph, "fft", "out");
        System.out.println("Part 2b: " + (p2a * p2b * p2c));
    }

    // function that takes a List of Sets of Integer and returns a Set of Integer
    private Set<Integer> intersection(List<Set<Integer>> sets) {
        return sets.stream().reduce((a, b) -> {
            a.retainAll(b);
            return a;
        }).get();
    }

    // function that takes a file name and returns the contents of that file as a
    // list of strings
    private List<String> readFile(String fileName) {
        List<String> lines = null;
        try {
            lines = Files.readAllLines(Paths.get(fileName));
        } catch (IOException e) {
            e.printStackTrace();
        }
        return lines;
    }

    // main
    public static void main(String[] args) {
        Day11 p = new Day11();
        p.part2();
    }
}
