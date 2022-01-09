import pygame
from spritesheet import SpriteSheet
from actor import find_actor, Babus, Archer, Templar, Mog
from pygame.time import Clock

def read_data() -> list:
    with open('moves.txt', 'r') as f:
        lines = f.read().split('\n')
        board = list()
        moves = list()

        state = 0
        for l, line in enumerate(lines):
            if not line:
                state += 1
            elif state == 0:
                board.append(line)
            elif state == 1:
                moves.append(eval(line))
    return board, moves

def screen_coords(x, y):
    sx = ssize//2 + x*ssize//2 + y*ssize//2
    sy = y*ssize//4 - x*ssize//4 + 228
    return (sx, sy)

symbol_actor = {'B': Babus, 'A': Archer, 'D': Templar, 'C': Mog}
ssize = 64

def run(board, moves: list):
    pygame.init()
    pygame.font.init()

    step = 805

    size = 640, 480
    screen = pygame.display.set_mode(size)

    myfont = pygame.font.SysFont('verdana', 32)

    actors = make_actors(board)

    sprites = SpriteSheet('Cubes - Green 64x64.png')
    sprite_rect = (0, 0, ssize, ssize)
    tile = sprites.image_at(sprite_rect, colorkey=-1)

    sprites = SpriteSheet('Cubes - Stone 64x64.png')
    rock = sprites.image_at(sprite_rect, colorkey=-1)
    move_iter = iter(moves)

    animation_event = pygame.event.custom_type()
    pygame.time.set_timer(animation_event, 250)
    movement_event = pygame.event.custom_type()
    pygame.time.set_timer(movement_event, 1000//120)
    ticker = Clock()

    actual_total = 0
    shown_total = 0

    running = True
    moving = True

    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pygame.quit()
                running = False
                break
            elif event.type == pygame.KEYDOWN:
                # Was it the Escape key? If so, stop the loop.
                if event.key == pygame.K_ESCAPE:
                    running = False
                    break
            elif event.type == animation_event:
                for actor in actors:
                    actor.animate()
            elif event.type == movement_event:
                if moving:
                    actors_in_motion = [actor for actor in actors if actor.moving]

                    if not actors_in_motion:
                        try:
                            move = next(move_iter)
                            print (move)
                            actual_total += move[-1]
                            actor = find_actor(actors, move)
                            actor.move_to(*move)
                        except StopIteration:
                            moving = False
                            pygame.time.set_timer(pygame.QUIT, 5000)

                    else:
                        for actor in actors_in_motion:
                            actor.update()

        if not running:
            break

        ticker.tick(60)
        screen.fill((0, 255, 255))

        if shown_total < actual_total:
            shown_total = min(actual_total, shown_total+83)

        textsurface = myfont.render(f"Part 2: {shown_total:05}", False, (255, 0, 0))
        screen.blit(textsurface, (5, 5))

        for y, row in enumerate(board):
            for x in range(len(row)-1, -1, -1):
                c = row[x]
                sx, sy = screen_coords(x, y)
                if c == '#':
                    screen.blit(rock, (sx, sy-8))
                elif c != ' ':
                    screen.blit(tile, (sx, sy))

        for actor in sorted(actors):
            sx, sy = screen_coords(actor.x, actor.y)
            sprite = actor.get_sprite()
            rects = actor.get_rect()
            screen.blit(
                sprite, (sx + (ssize-rects[2])//2, sy-rects[3]+ssize//4))

        pygame.display.flip()
        pygame.image.save(screen, f'puzzle23_{step}.png')
        step += 1


def make_actors(board: list) -> list:
    actors = list()
    for y, row in enumerate(board):
        for x, c in enumerate(row):
            if c in 'ABCD':
                actors.append(symbol_actor[c]((x, y)))
    return actors


board, moves = read_data()

run(board, moves)
