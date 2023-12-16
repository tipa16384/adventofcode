from collections import defaultdict

def beam(grid: list, energized: dict, coord: tuple, direction: tuple) -> int:
    while coord[0] >= 0 and coord[0] < len(grid[0]) and coord[1] >= 0 \
            and coord[1] < len(grid) and direction not in energized[coord]:
        energized[coord].append(direction)
        match grid[coord[1]][coord[0]]:
            case '/': direction = (-direction[1], -direction[0])
            case '\\': direction = (direction[1], direction[0])
            case '-':
                match direction:
                    case (0,1) | (0,-1):
                        beam(grid, energized, (coord[0]+1, coord[1]), (1,0))
                        direction = (-1,0)
            case '|':
                match direction:
                    case (1,0) | (-1,0):
                        beam(grid, energized, (coord[0], coord[1]+1), (0,1))
                        direction = (0,-1)
        coord = (coord[0]+direction[0], coord[1]+direction[1])
    return len(energized)

def read_data():
    with open('puzzle16.dat') as f:
        return f.read().splitlines()

def part1():
    print ("Part 1:", beam(read_data(), defaultdict(list), (0,0), (1,0)))
    
def part2():
    grid = read_data()
    beam_starts = [((x,0), (0,1)) for x in range(len(grid[0]))] + \
        [((0,y), (1,0)) for y in range(len(grid))] + \
            [((x,len(grid)-1), (0,-1)) for x in range(len(grid[0]))] + \
                [((len(grid[0])-1,y), (-1,0)) for y in range(len(grid))]
    print ("Part 2:", max(beam(grid, defaultdict(list), coord, direction)
        for coord, direction in beam_starts))
    
part1()

# time part2 using timeit
import timeit
print ("Part 2:", timeit.timeit("part2()", setup="from __main__ import part2", number=1))
