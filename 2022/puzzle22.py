import re
import curses

def turn(current_direction, which_way: str):
    if which_way == 'R':
        return (current_direction + 1) % len(directions)
    else:
        return (current_direction - 1) % len(directions)

def next_position(current_position, current_direction):
    new_position = current_position
    while True:
        new_position = (new_position[0] + directions[current_direction][0], new_position[1] + directions[current_direction][1])
        if new_position[1] < 0:
            new_position = (new_position[0], len(data) - 1)
        elif new_position[1] >= len(data):
            new_position = (new_position[0], 0)
        if new_position[0] < 0:
            new_position = (len(data[new_position[1]]) - 1, new_position[1])
        elif new_position[0] >= len(data[new_position[1]]):
            new_position = (0, new_position[1])
        char_at = data[new_position[1]][new_position[0]]
        match char_at:
            case ' ': continue
            case '#': return current_position, current_direction
            case '.': return new_position, current_direction

face_change = 'none yet'

def next_cube_position(current_position, current_direction):
    global face_change
    new_position = current_position
    face_change = ''

    new_position = (current_position[0] + directions[current_direction][0], current_position[1] + directions[current_direction][1])
    char_at = data[new_position[1]][new_position[0]]

    if char_at == ' ':
        # A -  51,   1, 100,  50
        # B - 101,   1, 150,  50
        # C -  51,  51, 100, 100
        # D -   1, 101,  50, 150
        # E -  51, 101, 100, 150
        # F -   1, 151,  50, 200

        # okay, we have to transform...
        if current_direction == 0:
            if new_position[1] < 51:
                # face B to face E
                face_change = "Face B to face E"
                current_direction = 2
                new_position = (100, 151 - new_position[1])
            elif new_position[1] < 101:
                # face C to face B
                face_change = "Face C to face B"
                current_direction = 3
                new_position = (new_position[1] + 50, 50)
            elif new_position[1] < 151:
                # face E to face B
                face_change = "Face E to face B"
                current_direction = 2
                new_position = (150, 151 - new_position[1])
            else:
                # face F to face E
                face_change = "Face F to face E"
                current_direction = 3
                new_position = (new_position[1] - 100, 150)
        elif current_direction == 1:
            if new_position[1] == 51:
                # face B to face C
                face_change = "Face B to face C"
                current_direction = 2
                new_position = (100, new_position[0] - 50)
            elif new_position[1] == 151:
                # face E to face F
                face_change = "Face E to face F"
                current_direction = 2
                new_position = (50, new_position[0] + 100)
            else:
                # face F to face B
                face_change = "Face F to face B"
                new_position = (new_position[0] + 100, 1)
        elif current_direction == 2:
            if new_position[1] < 51:
                # face A to face D
                face_change = "Face A to face D"
                current_direction = 0
                new_position = (1, 151 - new_position[1])
            elif new_position[1] < 101:
                # face C to face D
                face_change = "Face C to face D"
                current_direction = 1
                new_position = (new_position[1] - 50, 101)
            elif new_position[1] < 151:
                # face D to face A
                face_change = "Face D to face A"
                current_direction = 0
                new_position = (51, 151 - new_position[1])
            else:
                # face F to face A
                face_change = "Face F to face A"
                current_direction = 1
                new_position = (new_position[1] - 100, 1)
        else:
            if new_position[1] == 0 and new_position[0] < 101:
                # face A to face F
                face_change = "Face A to face F"
                current_direction = 0
                new_position = (1, new_position[0] + 100)
            elif new_position[1] == 0:
                # face B to face F
                face_change = "Face B to face F"
                new_position = (new_position[0] - 100, 200)
            else:
                # face D to face C
                face_change = "Face D to face C"
                current_direction = 0
                new_position = (51, new_position[0] + 50)

    char_at = data[new_position[1]][new_position[0]]
    if char_at == '#':
        return current_position, current_direction
    else:
        return new_position, current_direction

def read_input():
    with open(r'2022\puzzle22.txt', 'r') as f:
        data, moves = f.read().split('\n\n')
        data = data.splitlines()
        max_width = max([len(line) for line in data])
        # make every line in data max_width long filling with spaces
        data = [line.ljust(max_width) for line in data]

    return data, moves

def run_puzzle(moves, current_position, fnext):
    current_direction = 0

    for command in re.findall(r'[LR]|\d+', moves):
        #print (1000 * (current_position[1] - 1) + 4 * (current_position[0] - 1) + current_direction, current_position, current_direction, command)
        match command:
            case 'L': current_direction = turn(current_direction, 'L')
            case 'R': current_direction = turn(current_direction, 'R')
            case _:
                for _ in range(int(command)):
                    #print (1000 * (current_position[1] - 1) + 4 * (current_position[0] - 1) + current_direction, current_position, current_direction, command)
                    current_position, current_direction = fnext(current_position, current_direction)

    return 1000 * (current_position[1] - 1) + 4 * (current_position[0] - 1) + current_direction

directions = [(1, 0), (0, 1), (-1, 0), (0, -1)]
data, moves = read_input()
current_direction = 0
current_position, current_direction = next_position((0,0), current_direction)

print ("Part 1:", run_puzzle(moves, current_position, next_position))

print (re.findall(r'[LR]|\d+', moves))

current_direction = 0
current_position = (50,1)

data = [' ' + line + ' ' for line in data]
data = [' ' * len(data[0])] + data + [' ' * len(data[0])]

# window = curses.initscr()
# curses.noecho()
# curses.cbreak()
# window.keypad(True)

# while True:
#     window.clear()
#     # data is 152x202. window is 80x25. draw data with the current_position centered on the screen
#     low_y = max(0, current_position[1] - 12)
#     high_y = min(202, current_position[1] + 12)
#     low_x = max(0, current_position[0] - 39)
#     high_x = min(152, current_position[0] + 39)
#     for y in range(low_y, high_y):
#         window.addstr(y - low_y + 2, 0, data[y][low_x:high_x])
#         # draw current position
#         if y == current_position[1]:
#             window.addstr(y - low_y + 2, current_position[0] - low_x, '@')
#     # along the top of the screen, put the current_position and the current_direction
#     window.addstr(0, 0, f'current_position: {current_position}, current_direction: {current_direction}')
#     # top right of screen put face_change
#     window.addstr(0, 60, face_change)

#     # get user input character
#     key = window.getch()
#     if key == 113:
#         break
#     # else if key is left arrow
#     elif key == 452:
#         current_direction = 2
#     elif key == 454:
#         current_direction = 0
#     elif key == 450:
#         current_direction = 3
#     elif key == 456:
#         current_direction = 1
    
#     current_position, current_direction = next_cube_position(current_position, current_direction)

#     window.refresh()


# curses.nocbreak()
# window.keypad(False)
# curses.echo()
# curses.endwin()

# # A -  51,   1, 100,  50
# # B - 101,   1, 150,  50
# # C -  51,  51, 100, 100
# # D -   1, 101,  50, 150
# # E -  51, 101, 100, 150
# # F -   1, 151,  50, 200

# # Face A tests
# npos, ndir = next_cube_position((51, 1), 3)
# print (npos, ndir)
# assert npos == (1, 151)
# assert ndir == 0

# npos, ndir = next_cube_position((100, 1), 3)
# print (npos, ndir)
# assert npos == (1, 200)
# assert ndir == 0

# npos, ndir = next_cube_position((51, 1), 2)
# print (npos, ndir)
# assert npos == (1, 150)
# assert ndir == 0

# npos, ndir = next_cube_position((51, 50), 2)
# print (npos, ndir)
# assert npos == (1, 101)
# assert ndir == 0

# # Face B tests
# npos, ndir = next_cube_position((150, 1), 0)
# print (npos, ndir)
# assert npos == (100, 150)
# assert ndir == 2

# npos, ndir = next_cube_position((150, 50), 0)
# print (npos, ndir)
# assert npos == (100, 101)
# assert ndir == 2

# npos, ndir = next_cube_position((101, 50), 1)
# print (npos, ndir)
# assert npos == (100, 51)
# assert ndir == 2

# npos, ndir = next_cube_position((150, 50), 1)
# print (npos, ndir)
# assert npos == (100, 100)
# assert ndir == 2

# npos, ndir = next_cube_position((101, 1), 3)
# print (npos, ndir)
# assert npos == (1, 200)
# assert ndir == 3

# npos, ndir = next_cube_position((150, 1), 3)
# print (npos, ndir)
# assert npos == (50, 200)
# assert ndir == 3

# # Face C tests
# npos, ndir = next_cube_position((100, 51), 0)
# print (npos, ndir)
# assert npos == (101, 50)
# assert ndir == 3

# npos, ndir = next_cube_position((100, 100), 0)
# print (npos, ndir)
# assert npos == (150, 50)
# assert ndir == 3

# npos, ndir = next_cube_position((51, 51), 2)
# print (npos, ndir)
# assert npos == (1, 101)
# assert ndir == 1

# npos, ndir = next_cube_position((51, 100), 2)
# print (npos, ndir)
# assert npos == (50, 101)
# assert ndir == 1

# # Face D tests
# npos, ndir = next_cube_position((1, 101), 2)
# print (npos, ndir)
# assert npos == (51, 50)
# assert ndir == 0

# npos, ndir = next_cube_position((1, 150), 2)
# print (npos, ndir)
# assert npos == (51, 1)
# assert ndir == 0

# npos, ndir = next_cube_position((1, 101), 3)
# print (npos, ndir)
# assert npos == (51, 51)
# assert ndir == 0

# npos, ndir = next_cube_position((50, 101), 3)
# print (npos, ndir)
# assert npos == (51, 100)
# assert ndir == 0

# # Face E tests
# npos, ndir = next_cube_position((100, 101), 0)
# print (npos, ndir)
# assert npos == (150, 50)
# assert ndir == 2

# npos, ndir = next_cube_position((100, 150), 0)
# print (npos, ndir)
# assert npos == (150, 1)
# assert ndir == 2

# npos, ndir = next_cube_position((51, 150), 1)
# print (npos, ndir)
# assert npos == (50, 151)
# assert ndir == 2

# npos, ndir = next_cube_position((100, 150), 1)
# print (npos, ndir)
# assert npos == (50, 200)
# assert ndir == 2

# # Face F tests
# npos, ndir = next_cube_position((50, 151), 0)
# print (npos, ndir)
# assert npos == (51, 150)
# assert ndir == 3

# npos, ndir = next_cube_position((50, 200), 0)
# print (npos, ndir)
# assert npos == (100, 150)
# assert ndir == 3

# npos, ndir = next_cube_position((1, 200), 1)
# print (npos, ndir)
# assert npos == (101, 1)
# assert ndir == 1

# npos, ndir = next_cube_position((50, 200), 1)
# print (npos, ndir)
# assert npos == (150, 1)
# assert ndir == 1

# npos, ndir = next_cube_position((1, 151), 2)
# print (npos, ndir)
# assert npos == (51, 1)
# assert ndir == 1

# npos, ndir = next_cube_position((1, 200), 2)
# print (npos, ndir)
# assert npos == (100, 1)
# assert ndir == 1


# let's reset
print ("Part 2:", run_puzzle(moves, (51,1), next_cube_position))
