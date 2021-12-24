#############
#01.2.3.4.56#         <- hallway nodes
###D#C#D#B### 0 2 4 6 <- home nodes
  #C#A#A#B#   1 3 5 7
  #########

data = ["#01.2.3.4.56#", 
        "###A#B#C#D###", 
        "  #A#B#C#D#  ",
        "  #########  "]

class Node:
    def __init__(self, name, coord, home_for: str, contains: str):
        self.home_for = home_for
        self.connections = list()
        self.contains = contains
        self.name = name
        self.coord = coord
    
    def __repr__(self):
        return f"{self.name}({self.home_for}) -> {self.connections}"

class Connection:
    def __init__(self, to_node: Node, distance: int, blocked_by: list):
        self.blocked_by = blocked_by
        self.to_node = to_node
        self.distance = distance

hallway_nodes = list()
home_nodes = list()
map_width = len(data[0])

for y in range(len(data)):
    for x in range(map_width):
        c = data[y][x]
        if c in 'ABCD':
            home_nodes.append(Node(c, (x,y), data[y][x], data[y][x]))
        elif c in '0123456':
            hallway_nodes.append(Node(c, (x,y), None, None))

print (f"Home nodes: {home_nodes}")
print (f"Hallway nodes: {hallway_nodes}")

for h in hallway_nodes:
    for n in home_nodes:
        connection = Connection(n, abs(h.coord[0] - n.coord[0]) + abs(h.coord[1] - n.coord[1]), list())
        h.connections.append(connection)
        step_dir = -1 if h.coord[0] > n.coord[0] else 1
        start_x = h.coord[0]
        while start_x != n.coord[0]:
            start_x += step_dir
            if data[h.coord[1]][start_x] in '0123456':
                connection.blocked_by.append(hallway_nodes[int(data[h.coord[1]][start_x])])
        start_y = h.coord[1]
        while start_y != n.coord[1]:
            start_y += 1
            if data[start_y][h.coord[0]] in '0123456':
                connection.blocked_by.append(hallway_nodes[int(data[start_y][h.coord[0]])])

        
