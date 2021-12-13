import curses
from time import sleep

def neighbors(x, y):
    for y0 in range(y-1,y+2):
        for x0 in range(x-1,x+2):
            if ((x,y) != (x0,y0)) and y0 >= 0 and y0 < 10 and x0 >= 0 and x0 < 10:
                yield x0, y0

def read_grid():
    with open("puzzle11.dat") as f:
        return [[ord(c) - ord('0') for c in line.strip()] for line in f]

def fire(grid,x,y):
    grid[y][x] += 1
    if grid[y][x] == 10:
        for nx, ny in neighbors(x, y):
            fire(grid, nx, ny)

def main(stdscr):
    grid = read_grid()
    assert len(grid) == 10

    saved_flash_count = 0
    flash_count = 0
    flash_together = 0

    curses.init_pair(1, curses.COLOR_GREEN, curses.COLOR_BLACK)
    curses.init_pair(2, curses.COLOR_GREEN, curses.COLOR_WHITE)
    curses.init_pair(3, curses.COLOR_YELLOW, curses.COLOR_BLACK)
    curses.init_pair(4, curses.COLOR_YELLOW, curses.COLOR_BLACK)
    curses.init_pair(5, curses.COLOR_BLUE, curses.COLOR_BLACK)
    green = curses.color_pair(1)
    sync_green = curses.color_pair(1) + curses.A_BOLD
    red = curses.color_pair(3)
    yellow = curses.color_pair(4)
    blue = curses.color_pair(5)

    height, width = stdscr.getmaxyx()
    stdscr.clear()
    for y in range(height-1):
        for x in range(width-1):
            stdscr.addstr(y, x, "~", blue + curses.A_DIM)
    s = "Press any key to start..."
    stdscr.addstr(height//2, (width-len(s))//2, s, yellow + curses.A_BOLD + curses.A_BLINK)
    stdscr.refresh()
    stdscr.getkey()

    win = curses.newwin(24, 40, (height-24)//2, (width-40)//2)

    for step in range(1,1000):
        win.clear()
        win.addstr(0,0,f"Step", green)
        win.addstr(0,5,f"{step}", green + curses.A_BOLD)
        if saved_flash_count > 0:
            s = f"{saved_flash_count} flashes @ step 100"
            win.addstr(0,40-len(s), s, green)
        else:
            s = "Counting flashes   "
            win.addstr(0,40-len(s), s, green)
            if step & 4:
                win.addstr(0,37, "...", green)
        for y in range(10):
            for x in range(10):
                fire(grid,x,y)
        if not flash_together and all(grid[y][x] > 9 for x in range(10) for y in range(10)):
            flash_together = step
        for y in range(10):
            for x in range(10):
                # if any neighbor is > 9, then it's a flash
                if any(grid[y0][x0] > 9 for x0, y0 in neighbors(x, y)):
                    bold = curses.A_BOLD
                else:
                    bold = curses.A_NORMAL

                if grid[y][x] > 9:
                    win.addstr(2+2*y,x*4,"    ",curses.A_REVERSE)
                    win.addstr(3+2*y,x*4,"    ",curses.A_REVERSE)
                    flash_count += 1
                    grid[y][x] = 0
                elif grid[y][x] > 5:
                    win.addstr(2+2*y,x*4,"****",bold+yellow)
                    win.addstr(3+2*y,x*4,"****",bold+yellow)
                elif grid[y][x] > 1:
                    win.addstr(2+2*y,x*4,"~~~~",curses.A_DIM+blue)
                    win.addstr(3+2*y,x*4,"~~~~",curses.A_DIM+blue)
        if step == 100:
            saved_flash_count = flash_count
        if flash_together > 0:
            s = f"Synced at step {flash_together}"
            win.addstr(12,(40-len(s))//2,s, sync_green)
            break
        win.refresh()
        sleep(0.1)

    win.getkey()

curses.wrapper(main)
