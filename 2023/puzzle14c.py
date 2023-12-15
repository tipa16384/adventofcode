import curses
import time

def read_data():
    with open("puzzle14.dat") as f:
        return f.read().splitlines()

def let_us_rock(data: list) -> list:
    # add a row of '#' before and after data, and a '#' before and after each element in data
    data = ['#' + x + '#' for x in data]
    data.insert(0, '#' * (len(data[0])))
    data.append('#' * (len(data[0])))
    return data

def let_us_roll(data: list, offset: tuple) -> bool:
    # calculate a new list of round_rocks each offset by offset as long as the new round_rocks is not in rocks
    moved = False
    for y in range(1, len(data)-1):
        for x in range(1, len(data[0])-1):
            if data[y][x] == 'O':
                if data[y+offset[1]][x+offset[0]] == '.':
                    data[y+offset[1]] = data[y+offset[1]][:x+offset[0]] + 'O' + data[y+offset[1]][x+offset[0]+1:]
                    data[y] = data[y][:x] + '.' + data[y][x+1:]
                    moved = True

    return moved

def score(data: list) -> int:
    top_row = len(data) - 1
    # calculate the score of the round_rocks
    return sum([top_row - y for y in range(1, len(data)-1) for x in range(1, len(data[0])-1) if data[y][x] == 'O'])

def rolling_rock(data: list, offset: tuple) -> int:
    # calculate the round_rocks after they have rolled down the rocks
    # return the round_rocks and the score of the round_rocks
    while True:
        moved = let_us_roll(data, offset)
        if not moved:
            break
    return score(data)

def spin(data: list) -> int:
    for offset in [(0,-1), (-1,0), (0,1), (1,0)]: # north, west, south, east
        score = rolling_rock(data, offset)
    return score

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
    data = let_us_rock(read_data())
    pretty_print(data)
    score = rolling_rock(data, (0,-1))
    pretty_print(data)

    print ("Part 1:", score)

def more_score(data: list, scores: list, target: int):
    while target > len(scores):
        score = spin(data)
        scores.append(score)
    return scores

# print print data
def pretty_print(data: list):
    for x in data:
        print(x)
    print()

def part2():
    data = let_us_rock(read_data())
    scores = []
    scores = more_score(data, scores, 10)
    print ("Score head", scores[:10])
    # use tortoise and hare algorithm to find the cycle in scores
    tortoise, ti = scores[:3], 0
    hare, hi = scores[1:4], 1
    while hare != tortoise:
        ti += 1
        hi += 2
        scores = more_score(data, scores, hi+3)
        tortoise, hare = scores[ti:ti+3], scores[hi:hi+3]
    print ("Met up at", ti, hi, tortoise, hare)
    hi = ti + 1
    scores = more_score(data, scores, hi+3)
    hare = scores[hi:hi+3]
    while hare != tortoise:
        hi += 1
        scores = more_score(data, scores, hi+3)
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
