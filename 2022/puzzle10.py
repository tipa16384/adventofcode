def run_program(commands):
    command_list = {'noop': (1, lambda x, _: x),
                    'addx': (2, lambda x, y: x + y)}
    x, clock = 1, 1
    program_state = [(0, 0)]

    for command in commands:
        toks = command.split()
        op = toks[0]
        arg = int(toks[1]) if len(toks) == 2 else None
        x = command_list[op][1](x, arg)
        clock += command_list[op][0]
        program_state.append((clock, x))

    return program_state


def state_at(program_state, t):
    for i in range(len(program_state) - 1):
        if program_state[i][0] <= t < program_state[i + 1][0]:
            return program_state[i][1]
    return program_state[-1][1]


def solve_part1(program_state: list):
    part1 = sum([state_at(program_state, clock) *
                clock for clock in [20, 60, 100, 140, 180, 220]])
    print("Part 1: {}".format(part1))


def solve_part2(program_state: list):
    width, height = 40, 6
    sprite_pos = [state_at(program_state, pixel+1)
                  for pixel in range(width * height)]
    screen = ['#' if sprite_pos[pixel]-1 <=
              (pixel % width) <= sprite_pos[pixel]+1 else '.' for pixel in range(width * height)]

    print("Part 2:")
    # print 6 lines of screen at 40 characters each line
    for i in range(height):
        print(''.join(screen[i*width:(i+1)*width]))


with open(r"2022\puzzle10.txt") as f:
    data = f.read().splitlines()

program_state = run_program(data)

solve_part1(program_state)
solve_part2(program_state)
