from collections import defaultdict

with open('puzzle3.dat', 'r') as f:
    data = f.read().strip()

deltas = { '^': (0, -1), 'v': (0, 1), '<': (-1, 0), '>': (1, 0) }

visited = defaultdict(int)

pos = (0, 0)
visited[pos] += 1

for order in data:
    delta = deltas[order]
    pos = (pos[0] + delta[0], pos[1] + delta[1])
    visited[pos] += 1

print ('Part 1:', len(visited))

pos = (0, 0)
robo_pos = (0, 0)
visited = defaultdict(int)
visited[pos] += 2

for i in range(len(data)):
    if i % 2 == 0:
        delta = deltas[data[i]]
        pos = (pos[0] + delta[0], pos[1] + delta[1])
        visited[pos] += 1
    else:
        delta = deltas[data[i]]
        robo_pos = (robo_pos[0] + delta[0], robo_pos[1] + delta[1])
        visited[robo_pos] += 1

print ('Part 2:', len(visited))

