from timeit import default_timer as timer
from collections import defaultdict
import pygame
import sys
from pygame.time import Clock
from random import randint
from statistics import median

def read_sparse_data() -> set:
    with open('e:\\Documents\\adventofcode\\2015\\puzzle18.dat') as f:
        return set((x, y) for y, row in enumerate(f.read().split('\n')) \
            for x, cell in enumerate(row) if cell == '#')

def random_color():
    return randint(0, 255), randint(0, 255), randint(0, 255)

def make_neighbor_map(cells: set, cell_colors: dict, is_part_two: bool) -> set:
    neighbor_map = defaultdict(int)
    neighbor_color_map = defaultdict(list)
    for x, y in cells:
        neighbor_color_map[(x, y)].append(cell_colors[(x, y)])
        neighbor_color_map[(x, y)].append(cell_colors[(x, y)])
        neighbor_color_map[(x, y)].append(cell_colors[(x, y)])
        for nx, ny in [(dx, dy) for dy in range(y-1, y+2) \
            for dx in range(x-1, x+2) \
                if (dx, dy) != (x, y) and dx >= 0 and dx < 100 and dy >= 0 and dy < 100]:
            neighbor_map[(nx, ny)] += 1
            neighbor_color_map[(nx, ny)].append(cell_colors[(x, y)])
    if is_part_two:
        neighbor_map[(0, 0)] = neighbor_map[(99, 0)] = neighbor_map[(
            0, 99)] = neighbor_map[(99, 99)] = 3
        white = (255, 255, 255)
        neighbor_color_map[(0,0)].append(white)
        neighbor_color_map[(99,0)].append(white)
        neighbor_color_map[(0,99)].append(white)
        neighbor_color_map[(99,99)].append(white)
        
    return neighbor_map, neighbor_color_map

def average_color(colors: list) -> tuple:
    return tuple(median(x) for x in zip(*colors))

def next_generation(cells: set, cell_colors: dict, is_part_two: bool) -> set:
    new_cells = set()
    new_cell_colors = dict()
    neighbor_map, neighbor_color_map = make_neighbor_map(cells, cell_colors, is_part_two)
    for y in range(100):
        for x in range(100):
            neighbor_count = neighbor_map[(x, y)]
            colors = neighbor_color_map[(x, y)]
            neighbor_color = average_color(colors)
            if neighbor_count == 3 or ((neighbor_count == 2 and (x, y) in cells)):
                new_cells.add((x, y))
                new_cell_colors[(x, y)] = neighbor_color
    return new_cells, new_cell_colors

def simulate(cells: set, steps: int, is_part_two:bool=False) -> int:
    for _ in range(steps):
        cells, _ = next_generation(cells, defaultdict(list), is_part_two)
    return len(cells)

def pygame_simulate(cells: set, steps: int, is_part_two:bool=False) -> int:
    pygame.init()
    pygame.font.init()

    myfont = pygame.font.SysFont('monospace', 15)

    size = 500, 500
    screen = pygame.display.set_mode(size)
    ticker = Clock()

    cell_colors = {(x, y): random_color() for x, y in cells}

    for step in range(steps):
        cells, cell_colors = next_generation(cells, cell_colors, is_part_two)

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pygame.quit()
                sys.exit()
        ticker.tick(15)
        screen.fill((0, 0, 0))
        textsurface = myfont.render(f'Step: {step}', False, (255, 255, 255))
        screen.blit(textsurface, (0, 0))
        for x, y in cells:
            pygame.draw.circle(screen, cell_colors[(x,y)], (x*5+2.5, y*5+2.5), 4)
        pygame.display.flip()
        pygame.image.save(screen, f'e:\\Documents\\adventofcode\\2015\\puzzle18_pygame_{step}.png')

    return len(cells)


start = timer()
print(
    f"Part 1: {simulate(read_sparse_data(), 100)} in {timer() - start:.2f} seconds")
start = timer()
print(
    f"Part 2: {pygame_simulate(read_sparse_data(), 1000, True)} in {timer() - start:.2f} seconds")
