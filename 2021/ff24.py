import pygame
from spritesheet import SpriteSheet
from pygame.time import Clock


def read_data() -> list:
    with open('moves.txt', 'r') as f:
        lines = f.read().split('\n')
        num_moves = len(lines)//7
        moves = [[l for l in lines[i*7:i*7+7]] for i in range(num_moves)]
    return moves


def run(moves: list):
    pygame.init()
    pygame.font.init()

    size = 640, 480
    screen = pygame.display.set_mode(size)

    sprites = SpriteSheet('Cubes - Green 64x64.png')
    ssize = 64
    sprite_rect = (0, 0, ssize, ssize)
    tile = sprites.image_at(sprite_rect, colorkey=-1)

    sprites = SpriteSheet('Cubes - Stone 64x64.png')
    rock = sprites.image_at(sprite_rect, colorkey=-1)

    sprites = SpriteSheet('babus.png')
    babus_rects = [(12,  42, 32, 48), (12, 106, 32, 48),
                   (12, 170, 32, 48), (12, 234, 32, 48)]
    babus = sprites.images_at(babus_rects, colorkey=-1)

    sprites = SpriteSheet('archer.png')
    archer_rects = [(66,0,32,60), (66, 60, 32, 60), (98, 0, 32, 60), (98, 60, 32, 60)]
    archer = sprites.images_at(archer_rects, colorkey=-1)

    sprites = SpriteSheet('templar.png')
    templar_rects = [(12,34,32,56), (12,98,32,56), (12,162,32,56), (12,226,32,56)]
    templar = sprites.images_at(templar_rects, colorkey=-1)

    sprites = SpriteSheet('mog.png')
    mog_rects = [(198,154,32,56), (215,2,32,56), (198,210,32,56), (198,58,32,56)]
    mog = sprites.images_at(mog_rects, colorkey=-1)

    ticker = Clock()

    running = True
    while running:
        for data in moves:
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    pygame.quit()
                    running = False
                    break
                if event.type == pygame.KEYDOWN:
                    # Was it the Escape key? If so, stop the loop.
                    if event.key == pygame.K_ESCAPE:
                        running = False
                        break
            if not running: break
            screen.fill((0, 255, 255))
            ticker.tick(1)
            for y, row in enumerate(data):
                for x in range(len(row)-1, -1, -1):
                    c = row[x]
                    sx = ssize//2 + x*ssize//2 + y*ssize//2
                    sy = y*ssize//4 - x*ssize//4 + 228
                    if c == '#':
                        screen.blit(rock, (sx, sy-8))
                    elif c in 'ABCD.':
                        facing = 0 if y == 1 else 3
                        screen.blit(tile, (sx, sy))
                        if c != '.':
                            if c == 'A':
                                sprite = archer[facing]
                                rects = archer_rects[facing]
                            elif c == 'C':
                                sprite = templar[facing]
                                rects = templar_rects[facing]
                            elif c == 'D':
                                sprite = mog[facing]
                                rects = mog_rects[facing]
                            else:
                                sprite = babus[facing]
                                rects = babus_rects[facing]
                            screen.blit(
                                sprite, (sx + (ssize-rects[2])//2, sy-rects[3]+ssize//4))
            pygame.display.flip()


moves = read_data()

run(moves)
