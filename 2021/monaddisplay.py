import pygame
from pygame.time import Clock
import threading
from time import sleep

def read_program() -> list:
    with open('e:\\Documents\\adventofcode\\2021\\puzzle24.dat') as f:
        return f.read().split('\n')

def analyze_input(program: list) -> None:
    current_input, z_stack, ins = -1, list(), (x for x in program)
    for instruction in ins:
        if instruction.startswith('inp'): current_input += 1
        elif instruction == 'div z 1':
            for _ in range(11): xins = next(ins)
            z_stack.append((current_input, int(xins.split()[-1])))
        elif instruction == 'div z 26':
            left, xins = z_stack.pop()
            yield left, current_input, xins + int(next(ins).split()[-1])

def push_display(display: list, lineno: int) -> None:
    display_lines[lineno] = ''.join(display)
    sleep(0.1)

def push_line(s: str, lineno: int) -> None:
    display_lines[lineno] = s
    sleep(0.1)

def hack_program(program: list, digits: str, lineno: int) -> str:
    display = ['~'] * 14
    push_display(display, lineno)

    for left, right, offset in analyze_input(program):
        for c in digits:
            rc = str(int(c) + offset)
            display[left], display[right] = c, (rc if rc in digits else '~')
            push_display(display, lineno)
            if len(rc) == 1 and rc in digits:
                break

    return ''.join(display)

def run_terminal():
    pygame.init()
    pygame.font.init()

    myfont = pygame.font.SysFont('dseg14classicitalic', 32)

    size = 500, 310
    screen = pygame.display.set_mode(size)
    ticker = Clock()

    running = True
    step = 0
    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pygame.quit()
                running = False
            if event.type == pygame.KEYDOWN:
            # Was it the Escape key? If so, stop the loop.
                if event.key == pygame.K_ESCAPE:
                    running = False     

        line_height = myfont.size('A')[1]+5

        ticker.tick(15)
        screen.fill((0, 0, 0))
        for il, line in enumerate(display_lines):
            center = False
            if line and line[0] == '`':
                line = line[1:]
                center = True
            textsurface = myfont.render(line, False, (0, 255, 0))
            textsurface.set_alpha(255)
            xoff, yoff = 10, 10 + il*line_height
            if center:
                width, _ = myfont.size(line)
                xoff = (size[0] - width) // 2
            screen.blit(textsurface, (xoff, yoff))
        pygame.display.flip()
        pygame.image.save(screen, f'e:\\Documents\\adventofcode\\2021\\puzzle24_{step}.png')
        step += 1

display_lines = [''] * 10

def main():
    x = threading.Thread(target=run_terminal)
    x.start()
    push_line('`Advent of Code', 0)
    for i in range(1, 25):
        push_line(f'`Day {i:02}, {min(2021, 2014+i)}', 1)
    program = read_program()
    push_line('Part 1:', 3)
    hack_program(program, '987654321', 4)
    push_line('Part 2:', 6)
    hack_program(program, '123456789', 7)

main()
