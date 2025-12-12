import java.util.*;

public class DagPathCounter {

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
            String end
    ) {
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
            if (u.equals(end)) continue;

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


    // ---------------- Example Usage ----------------
    public static void main(String[] args) {
        Map<String, List<String>> graph = new HashMap<>();

        graph.put("A", Arrays.asList("B", "C"));
        graph.put("B", Arrays.asList("D"));
        graph.put("C", Arrays.asList("D"));
        graph.put("D", Collections.emptyList());

        long paths = countPaths(graph, "A", "D");
        System.out.println("Paths A → D = " + paths); // 2
    }
}
