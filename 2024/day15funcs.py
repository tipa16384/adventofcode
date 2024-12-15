from enum import Enum

def day15data(file):
    grid, moves = file.read().decode('utf-8').split('\n\n')
    moves = moves.replace('\n', '')
    grid = [list(row) for row in grid.splitlines()]

    rocks = {(x, y) for y, row in enumerate(grid) for x, cell in enumerate(row) if cell == '#'}
    boxes = {(x, y) for y, row in enumerate(grid) for x, cell in enumerate(row) if cell == 'O'}
    robot = next((x, y) for y, row in enumerate(grid) for x, cell in enumerate(row) if cell == '@')

    return rocks, boxes, robot, moves

dir_map = { '>': (1, 0), '<': (-1, 0), '^': (0, -1), 'v': (0, 1) }

def part1(rocks, og_boxes, robot, moves):
    boxes = og_boxes.copy()
    for move in moves:
        dx, dy = dir_map[move]
        new_robot_pos = (robot[0] + dx, robot[1] + dy)
        
        if new_robot_pos in rocks:
            continue
        
        if new_robot_pos not in boxes:
            robot = new_robot_pos
            continue
        
        new_box_pos = new_robot_pos
        while new_box_pos in boxes:
            new_box_pos = (new_box_pos[0] + dx, new_box_pos[1] + dy)
        
        if new_box_pos in rocks:
            continue
        
        boxes.remove(new_robot_pos)
        boxes.add(new_box_pos)
        robot = new_robot_pos

    return sum(100 * y + x for x, y in boxes)

class Object(Enum):
    ROCK = '#'
    BOX = 'O'
    ROBOT = '@'
    EMPTY = '.'
    LEFT_BOX = '['
    RIGHT_BOX = ']'

def look_at(rocks, left_boxes, right_boxes, robot, x, y):
    xy = (x, y)
    if xy == robot:
        return Object.ROBOT
    if xy in rocks:
        return Object.ROCK
    if xy in left_boxes:
        return Object.LEFT_BOX
    if xy in right_boxes:
        return Object.RIGHT_BOX
    return Object.EMPTY

def can_move(rocks, left_boxes, right_boxes, robot, x, y, dx, dy, mass) -> tuple:
    i_am = look_at(rocks, left_boxes, right_boxes, robot, x, y)

    if i_am == Object.EMPTY:
        return True
    
    if i_am == Object.ROCK:
        return False
    
    mass.add((x, y, i_am))

    if not dy:
        return can_move(rocks, left_boxes, right_boxes, robot, x + dx, y + dy, dx, dy, mass)
    
    look = look_at(rocks, left_boxes, right_boxes, robot, x + dx, y + dy)

    # dy <> 0 and object is a left or right box
    if look == Object.LEFT_BOX:
        return can_move(rocks, left_boxes, right_boxes, robot, x, y + dy, dx, dy, mass) and \
            can_move(rocks, left_boxes, right_boxes, robot, x + 1, y + dy, dx, dy, mass)
    if look == Object.RIGHT_BOX:
        return can_move(rocks, left_boxes, right_boxes, robot, x, y + dy, dx, dy, mass) and \
            can_move(rocks, left_boxes, right_boxes, robot, x - 1, y + dy, dx, dy, mass)

    return can_move(rocks, left_boxes, right_boxes, robot, x + dx, y + dy, dx, dy, mass)

def move_obj(left_boxes: set, right_boxes: set, mass: set, dx, dy):
    # remove set mass from sets left_boxes and right_boxes using set difference
    left_boxes -= {(x, y) for x, y, _ in mass}
    right_boxes -= {(x, y) for x, y, _ in mass}
    # add mass to left and right boxes, moving them dx, dy
    left_boxes |= {(x + dx, y + dy) for x, y, kind in mass if kind == Object.LEFT_BOX}
    right_boxes |= {(x + dx, y + dy) for x, y, kind in mass if kind == Object.RIGHT_BOX}

def part2(og_rocks, og_boxes, robot, moves):
    rocks = {(x * 2, y) for x, y in og_rocks} | {(x * 2 + 1, y) for x, y in og_rocks}
    left_boxes = {(x * 2, y) for x, y in og_boxes}
    right_boxes = {(x * 2 + 1, y) for x, y in og_boxes}
    robot = (robot[0] * 2, robot[1])

    for move in moves:
        mass = set()
        dx, dy = dir_map[move]
        if can_move(rocks, left_boxes, right_boxes, robot, *robot, dx, dy, mass):
            move_obj(left_boxes, right_boxes, mass, dx, dy)
            robot = (robot[0] + dx, robot[1] + dy)

    return sum(100 * y + x for x, y in left_boxes)
