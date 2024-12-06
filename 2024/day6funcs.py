import re
from itertools import cycle
import time
from concurrent.futures import ProcessPoolExecutor

def day6_data(file) -> tuple:
    lines = file.read().decode('utf-8').splitlines()
    stuff = set([(row, m.start()) for row, line in enumerate(lines) for m in re.finditer('#', line)])
    start_pos = next((row, line.index('^')) for row, line in enumerate(lines) if '^' in line)
    return stuff, start_pos, len(lines), len(lines[0])

def find_path(stuff, start_pos, grid_height, grid_width) -> int:
    travel_offset = cycle([(-1, 0), (0, 1), (1, 0), (0, -1)])
    visited_cells = set()
    offset = next(travel_offset)
    pos = start_pos
    while True:
        visited_cells.add(pos)
        new_pos = (pos[0] + offset[0], pos[1] + offset[1])
        if new_pos in stuff:
            offset = next(travel_offset)
            continue
        if new_pos[0] < 0 or new_pos[0] >= grid_height or new_pos[1] < 0 or new_pos[1] >= grid_width:
            break
        pos = new_pos
    path_len = len(visited_cells)
    visited_cells.remove(start_pos)
    return path_len, visited_cells

def find_loop(stuff: list, start_pos: tuple, grid_height: int, grid_width: int) -> bool:
    travel_offsets = cycle([(-1, 0), (0, 1), (1, 0), (0, -1)])
    visited_cells = set()
    pos = start_pos
    offset = next(travel_offsets)
    
    while True:
        pos_offset = (pos[0], pos[1], offset)
        if pos_offset in visited_cells: return True
        visited_cells.add(pos_offset)
        new_pos = (pos[0] + offset[0], pos[1] + offset[1])
        
        if new_pos[0] < 0 or new_pos[0] >= grid_height or new_pos[1] < 0 or new_pos[1] >= grid_width:
            return False
        
        if new_pos in stuff:
            offset = next(travel_offsets)
            continue
        
        pos = new_pos

def find_obstructions(stuff, start_pos, grid_height, grid_width, obstructions) -> int:
    start_time = time.time()
    with ProcessPoolExecutor() as executor:
        threads = [executor.submit(find_loop, stuff | {obstruction}, start_pos, grid_height, grid_width) for obstruction in obstructions]
        obstruction_count = sum(1 for thread in threads if thread.result())
    end_time = time.time()
    print(f"Execution time: {end_time - start_time:.6f} seconds")
    return obstruction_count
