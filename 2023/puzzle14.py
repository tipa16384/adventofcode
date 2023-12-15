import curses
import time

def read_data():
    with open("puzzle14.dat") as f:
        return f.read().splitlines()

def let_us_rock(data: list) -> tuple:
    # record the (x,y) coordinates of every 'O' in the data
    round_rocks = {(x,y): data[y][x] == 'O' for y in range(len(data)) for x in range(len(data[y]))}
    # record the (x,y) coordinates of every '#' in the data
    rocks = [(x,y) for y in range(len(data)) for x in range(len(data[y])) if data[y][x] == '#']
    # record the (x,y) coordinates of each space bordering the data
    width, height = len(data[0]), len(data)
    border = [(x,y) for y in range(-1, height+1) for x in range(-1, width+1) if x == -1 or x == width or y == -1 or y == height]
    return (round_rocks, set(rocks+border), height)

def let_us_roll(round_rocks: dict, rocks: set, offset: tuple) -> tuple:
    # calculate a new list of round_rocks each offset by offset as long as the new round_rocks is not in rocks
    moved = False
    for xy in round_rocks.keys():
        if not round_rocks[xy]: continue
        moved_to = xy
        for i in range(1, 1000):
            new_pos = (xy[0] + i * offset[0], xy[1] + i * offset[1])
            if new_pos not in rocks and not round_rocks[new_pos]:
                moved_to = new_pos
            else:
                break
        if moved_to != xy:
            round_rocks[moved_to] = True
            round_rocks[xy] = False
            moved = True

    return (round_rocks, moved)

def score(round_rocks: dict, top_row: int) -> int:
    # calculate the score of the round_rocks
    return sum([top_row - xy[1] for xy in round_rocks if round_rocks[xy]])

def rolling_rock(round_rocks: list, rocks: set, top_row: int, offset: tuple) -> tuple:
    # calculate the round_rocks after they have rolled down the rocks
    # return the round_rocks and the score of the round_rocks
    while True:
        round_rocks, moved = let_us_roll(round_rocks, rocks, offset)
        if not moved:
            break
    return (round_rocks, score(round_rocks, top_row))

def spin(round_rocks: dict, rocks: set, top_row: int) -> tuple:
    for offset in [(0,-1), (-1,0), (0,1), (1,0)]: # north, west, south, east
        round_rocks, score = rolling_rock(round_rocks, rocks, top_row, offset)
    return (round_rocks, score)

def animate_rolling(round_rocks: dict, rocks: set, top_row: int) -> list:
    # using curses, animate the rolling of the round_rocks. Pause for 0.1 seconds between each frame.
    # wait for user to hit a key before ending simulation.
    stdscr = curses.initscr()
    curses.cbreak()
    curses.noecho()
    stdscr.keypad(True)
    stdscr.clear()
    # top_row is highest y coordinate in rocks
    rightmost_col = max([x for x,_ in rocks])

    part1_list = []
    for offset in [(0,-1), (-1,0), (0,1), (1,0)]*5:
        while True:
            stdscr.clear()
            for x,y in rocks:
                # if (y+1, x+1) is in screen bounds
                if y+1 >= 0 and y+1 < curses.LINES and x+1 >= 0 and x+1 < curses.COLS:
                    stdscr.addstr(y+1, x+1, '#')
            for xy in round_rocks:
                if not round_rocks[xy]: continue
                x,y = xy
                if y+1 >= 0 and y+1 < curses.LINES and x+1 >= 0 and x+1 < curses.COLS:
                    stdscr.addstr(y+1, x+1, 'O')
            # display the part1_list vertically to the right of the animation
            for i, p in enumerate(part1_list):
                stdscr.addstr(i+1, rightmost_col+3, str(p))
            stdscr.refresh()
            # pause briefly
            time.sleep(0.1)
            round_rocks, moved = let_us_roll(round_rocks, rocks, offset)
            if not moved:
                break
        if offset == (0,-1):
            # part1 answer is sum of the differences between the top_row and each y coordinate in round_rocks
            part1 = score(round_rocks, top_row)
            # add part1 to the head of part1_list and keep only the first 10 elements
            part1_list.insert(0, part1)
            part1_list = part1_list[:10]

    stdscr.getkey()
    curses.nocbreak()
    stdscr.keypad(False)
    curses.echo()
    curses.endwin()

    return round_rocks

def part1():
    print ("Part 1:", rolling_rock(*let_us_rock(read_data()), (0,-1))[1])

def more_score(round_rocks: dict, rocks: set, top_row: int, scores: list, target: int):
    while target > len(scores):
        round_rocks, score = spin(round_rocks, rocks, top_row)
        scores.append(score)
    return (round_rocks, scores)

def part2():
    round_rocks, rocks, top_row = let_us_rock(read_data())
    scores = []
    round_rocks, scores = more_score(round_rocks, rocks, top_row, scores, 10)
    print ("Score head", scores[:10])
    # use tortoise and hare algorithm to find the cycle in scores
    tortoise, ti = scores[:3], 0
    hare, hi = scores[1:4], 1
    while hare != tortoise:
        ti += 1
        hi += 2
        round_rocks, scores = more_score(round_rocks, rocks, top_row, scores, hi+3)
        tortoise, hare = scores[ti:ti+3], scores[hi:hi+3]
    print ("Met up at", ti, hi, tortoise, hare)
    hi = ti + 1
    round_rocks, scores = more_score(round_rocks, rocks, top_row, scores, hi+3)
    hare = scores[hi:hi+3]
    while hare != tortoise:
        hi += 1
        round_rocks, scores = more_score(round_rocks, rocks, top_row, scores, hi+3)
        hare = scores[hi:hi+3]
    print ("Cycle at", ti, hi, tortoise, hare)
    print ("Cycle length", hi-ti)
    xi = (1000000000 - ti) % (hi-ti)
    print ("Part 2:", scores[ti+xi-1])

# animate_rolling(*let_us_rock(read_data()))

# time part 1 and part 2
import timeit
print ("Part 1:", timeit.timeit("part1()", setup="from __main__ import part1", number=1))
print ("Part 2:", timeit.timeit("part2()", setup="from __main__ import part2", number=1))
