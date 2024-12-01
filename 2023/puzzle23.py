import networkx as nx

def read_grid(filename):
    grid = []
    with open(filename, 'r') as f:
        for line in f:
            grid.append(list(line.rstrip('\n')))
    return grid

def find_start_end(grid):
    start = None
    end = None
    # Top row (y = 0)
    y = 0
    for x, cell in enumerate(grid[y]):
        if cell == '.':
            start = (x, y)
            break
    # Bottom row (y = len(grid)-1)
    y = len(grid) - 1
    for x, cell in enumerate(grid[y]):
        if cell == '.':
            end = (x, y)
            break
    return start, end

def identify_nodes(grid, start, end):
    nodes = set()
    directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]  # (dx, dy)
    direction_labels = {(-1, 0): 'left', (1, 0): 'right', (0, -1): 'up', (0, 1): 'down'}
    opposite_directions = {('left', 'right'), ('right', 'left'), ('up', 'down'), ('down', 'up')}

    for y in range(len(grid)):
        for x in range(len(grid[0])):
            cell = grid[y][x]
            if cell == '#':
                continue
            if (x, y) == start or (x, y) == end:
                nodes.add((x, y))
                continue
            if cell in ['^', 'v', '<', '>']:
                nodes.add((x, y))
                continue
            neighbor_dirs = []
            for dx, dy in directions:
                nx, ny = x + dx, y + dy
                if 0 <= ny < len(grid) and 0 <= nx < len(grid[0]):
                    neighbor_cell = grid[ny][nx]
                    if neighbor_cell != '#':
                        neighbor_dirs.append(direction_labels[(dx, dy)])
            if len(neighbor_dirs) != 2:
                nodes.add((x, y))
            else:
                dir1 = neighbor_dirs[0]
                dir2 = neighbor_dirs[1]
                if (dir1, dir2) in opposite_directions or (dir2, dir1) in opposite_directions:
                    # Straight path, not a node
                    continue
                else:
                    # Path changes direction, so this is a node
                    nodes.add((x, y))
    return nodes

def build_edges(grid, nodes):
    edges = []
    edge_keys = set()
    directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]  # (dx, dy)
    slope_directions = {'^': (0, -1), 'v': (0, 1), '<': (-1, 0), '>': (1, 0)}
    for node in nodes:
        x, y = node
        cell = grid[y][x]
        # Determine possible directions to explore
        if cell in ['^', 'v', '<', '>']:
            # Forced direction due to slope
            dx, dy = slope_directions[cell]
            directions_to_explore = [(dx, dy)]
        else:
            # Can explore all accessible directions
            directions_to_explore = []
            for dx, dy in directions:
                nx, ny = x + dx, y + dy
                if 0 <= ny < len(grid) and 0 <= nx < len(grid[0]):
                    neighbor_cell = grid[ny][nx]
                    if neighbor_cell != '#':
                        directions_to_explore.append((dx, dy))

        for dx, dy in directions_to_explore:
            # Initialize
            current_x, current_y = x, y
            length = 0
            one_way = False
            current_dx, current_dy = dx, dy

            # If cell is a slope, movement is one-way
            if cell in ['^', 'v', '<', '>']:
                one_way = True

            while True:
                # Move to next cell
                current_x += current_dx
                current_y += current_dy
                length += 1
                # Check bounds
                if not (0 <= current_y < len(grid) and 0 <= current_x < len(grid[0])):
                    break
                next_cell = grid[current_y][current_x]
                if next_cell == '#':
                    break
                if (current_x, current_y) == node:
                    # Avoid loops back to the same node
                    break
                if (current_x, current_y) in nodes:
                    # Reached another node
                    end_node = (current_x, current_y)
                    if one_way:
                        edge_key = (node, end_node)
                    else:
                        # For two-way edges, use a frozenset to avoid duplicates
                        edge_key = frozenset([node, end_node])
                    if edge_key not in edge_keys:
                        edges.append({'start': node, 'end': end_node, 'length': length, 'one_way': one_way})
                        edge_keys.add(edge_key)
                    break
                # Handle slopes
                if next_cell in ['^', 'v', '<', '>']:
                    # Movement is forced in the direction of the slope
                    current_dx, current_dy = slope_directions[next_cell]
                    one_way = True
    return edges


def build_graph(start, end, edges):
    G = nx.DiGraph()
    # Add nodes
    G.add_node(start)
    G.add_node(end)
    # Add edges
    for edge in edges:
        start_node = edge['start']
        end_node = edge['end']
        length = edge['length']
        one_way = edge['one_way']
        # Add edge(s) based on one_way flag
        G.add_edge(start_node, end_node, weight=length)
        if not one_way:
            G.add_edge(end_node, start_node, weight=length)
    return G

def find_longest_path(G, start, end):
    # Generate all simple paths from start to end
    # log heading into networkx
    print ("Finding all simple paths from start to end")
    all_paths = nx.all_simple_paths(G, source=start, target=end)
    # log coming out
    print ("All simple paths found")
    max_length = 0
    longest_path = None
    for path in all_paths:
        # Calculate the total length of the path
        length = sum(G[path[i]][path[i+1]]['weight'] for i in range(len(path)-1))
        print ("Path length is ", length)
        if length > max_length:
            max_length = length
            longest_path = path
    return longest_path, max_length

def main():
    grid = read_grid('puzzle23.dat')
    start, end = find_start_end(grid)
    nodes = identify_nodes(grid, start, end)
    edges = build_edges(grid, nodes)
    # print start node, end node, and number of edges
    print(f"Start node: {start}")
    print(f"End node: {end}")
    print(f"Number of edges: {len(edges)}")

    G = build_graph(start, end, edges)
    longest_path, max_length = find_longest_path(G, start, end)
    if longest_path:
        print(f"Longest path length: {max_length}")
        print(f"Longest path: {longest_path}")
    else:
        print("No path found from start to end.")

if __name__ == '__main__':
    main()