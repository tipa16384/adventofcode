from math import sqrt

def walk_the_snake(puzzle: list, rope_size: int) -> int:
    snake = [(0,0)] * rope_size
    dir_map = {'U': (0, -1), 'D': (0, 1), 'L': (-1, 0), 'R': (1, 0)}
    tail_visited = set()
    tail_visited.add(snake[0])

    for command in puzzle:
        snake[-1] = (snake[-1][0] + command[1] * dir_map[command[0]][0], snake[-1][1] + command[1] * dir_map[command[0]][1])
        anything_moved = True
        while anything_moved:
            anything_moved = False
            for i in range(len(snake) - 1, 0, -1):
                head = snake[i]
                tail = snake[i-1]
                dist_from_tail = sqrt((head[0] - tail[0]) ** 2 + (head[1] - tail[1]) ** 2)
                if dist_from_tail < 2.0: break
                # tail takes one step towards head
                tail = (tail[0] + just_one_step(head[0], tail[0]), tail[1] + just_one_step(head[1], tail[1]))
                anything_moved = True
                snake[i-1] = tail
            tail_visited.add(snake[0])
    
    return len(tail_visited)

def code_gen(data: list):
    for l in data:
        t = l.split()
        yield t[0], int(t[1])

def just_one_step(a, b) -> int:
    return 0 if a == b else int(abs(a - b)/(a - b))

with open(r"2022\puzzle9.txt") as f:
    puzzle = [command for command in code_gen(f.read().splitlines())]

print ("Part 1: {}".format(walk_the_snake(puzzle, 2)))
print ("Part 2: {}".format(walk_the_snake(puzzle, 10)))
