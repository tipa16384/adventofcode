def load_sum(a):
    rslt = 0
    current_load = 1
    for xI in range(len(a) - 1, -1, -1):
        for xJ in range(len(a[xI])):
            if a[xI][xJ] == 'O':
                rslt += current_load
        current_load += 1
    return rslt


def show_board(a):
    for a_line in a:
        print(a_line)
    print("---")


def tilt_board(a, direction):
    def swap_tiles(x1, y1, x2, y2):
        c_temp_1 = a[x1][y1]
        c_temp_2 = a[x2][y2]
        a[x1] = a[x1][:y1] + c_temp_2 + a[x1][y1 + 1:]
        a[x2] = a[x2][:y2] + c_temp_1 + a[x2][y2 + 1:]
    def move_as_far_as_possible(init_x, init_y, delta_x, delta_y):
        if not a[init_x][init_y] == 'O':
            return
        curr_x = init_x
        curr_y = init_y
        while 0 <= curr_x + delta_x < len(a) and 0 <= curr_y + delta_y < len(a[0]):
            if a[curr_x + delta_x][curr_y + delta_y] == '.':
                swap_tiles(curr_x, curr_y, curr_x + delta_x, curr_y + delta_y)
                curr_x += delta_x
                curr_y += delta_y
            else:
                break
    if direction == 'north':
        for xI in range(len(a)):
            for xJ in range(len(a[xI])):
                move_as_far_as_possible(xI, xJ, -1, 0)
    if direction == 'south':
        for xI in range(len(a) - 1, -1, -1):
            for xJ in range(len(a[xI])):
                move_as_far_as_possible(xI, xJ, +1, 0)
    if direction == 'east':
        for xJ in range(len(a[0]) - 1, -1, -1):
            for xI in range(len(a)):
                move_as_far_as_possible(xI, xJ, 0, +1)
    if direction == 'west':
        for xJ in range(len(a[0])):
            for xI in range(len(a)):
                move_as_far_as_possible(xI, xJ, 0, -1)

    cache_values = {".": 1, "#": 2, "O": 3}
    cache_directions = {"north": 1, "west": 2, "south": 3, "east": 4}
    cached_result = cache_directions[direction]
    for xI in range(len(a)):
        for xJ in range(len(a[0])):
            cached_result = cached_result * 10 + cache_values[a[xI][xJ]]
    return cached_result



with open("puzzle14.dat", 'r') as day14_file:
    day14_lines = day14_file.read().splitlines()

def part1():
    # show_board(day14_lines)
    caches = {tilt_board(day14_lines, 'north'): 1}
    # show_board(day14_lines)
    print("Phase 01 result:", load_sum(day14_lines))

def part2():
    caches = {tilt_board(day14_lines, 'north'): 1}
    rotation_cycle = ['north', 'west', 'south', 'east']
    current_rotation = 1
    cycles_done = 1
    cycles_limit = (10 ** 9) * 4 # damn, spent almost an hour debugging before realizing it's 4B rotations, not 1B
    warp_drive_initiated = False
    while cycles_done < cycles_limit:
        tilt_result = tilt_board(day14_lines, rotation_cycle[current_rotation])
        cycles_done += 1
        current_rotation += 1
        if current_rotation > 3:
            current_rotation = 0
            # show_board(day14_lines)
            # print(load_sum(day14_lines))
        if not tilt_result in caches:
            caches[tilt_result] = cycles_done
        else:
            # print(caches[tilt_result], cycles_done, load_sum(day14_lines), tilt_result)
            if not warp_drive_initiated:  # make a warp jump to the end
                delta = cycles_done - caches[tilt_result]
                multiplier = 10 ** 9
                while multiplier > 0:
                    while cycles_done + (delta * multiplier) < cycles_limit:
                        cycles_done += (delta * multiplier)
                    multiplier = multiplier // 10
                warp_drive_initiated = True
                # print("Warped to:", cycles_done)
    print("Phase 02 result:", load_sum(day14_lines))

# time part 1 and part 2
import timeit
print ("Part 1:", timeit.timeit("part1()", setup="from __main__ import part1", number=1))
print ("Part 2:", timeit.timeit("part2()", setup="from __main__ import part2", number=1))
