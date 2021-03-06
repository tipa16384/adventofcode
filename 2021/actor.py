from spritesheet import SpriteSheet
import pygame


class Actor:
    def __init__(self, sprites, rects, pos, facing=3):
        self.sprites = sprites
        self.pos = pos
        self.facing = facing
        self.rects = rects
        self.x, self.y = pos
        self.dx = 0
        self.dy = 0
        self.speed = 16
        self.moving = False
        self.animated = type(sprites[0]) == list
        self.move_queue = list()
        self.frame = 0

    def animate(self):
        self.frame += 1

    def get_rect(self):
        return self.rects[self.facing]

    def get_sprite(self):
        if not self.animated:
            return self.sprites[self.facing]
        else:
            return self.sprites[self.facing][self.frame % len(self.sprites[self.facing])]

    def update(self):
        if self.moving:
            self.move()
        else:
            self.dx = 0
            self.dy = 0
        self.x += self.dx
        self.y += self.dy
        self.pos = (self.x, self.y)

    def move(self):
        if self.move_queue:
            self.dx, self.dy = self.move_queue.pop(0)
            if self.dx:
                self.facing = 0 if self.dx < 0 else 1
            elif self.dy:
                self.facing = 2 if self.dy > 0 else 3
        else:
            self.moving = False
            self.dx, self.dy = 0, 0
            if self.y != 1:
                self.facing = 4
                self.frame = 0

    def move_to(self, x, y, x1, y1, score):
        if y == 1:
            dx = (1 if x1 > x else -1)/self.speed
            while x != x1:
                self.move_queue.append((dx, 0))
                x += dx
            dy = (1 if y1 > y else -1)/self.speed
            while y != y1:
                self.move_queue.append((0, dy))
                y += dy
        else:
            dy = (1 if y1 > y else -1)/self.speed
            while y != y1:
                self.move_queue.append((0, dy))
                y += dy
            dx = (1 if x1 > x else -1)/self.speed
            while x != x1:
                self.move_queue.append((dx, 0))
                x += dx
        self.moving = True

    def draw(self, screen):
        screen.blit(self.sprite, self.rect)

    def __lt__(self, other):
        return self.y < other.y or self.x > other.x


class Babus (Actor):
    def __init__(self, pos):
        sprites = SpriteSheet('babus.png')
        babus_rects = [(12, 42, 32, 48), (54, 42, 32, 48),
                       (96, 42, 32, 48), (138, 42, 32, 48)]
        babus_l = sprites.images_at(babus_rects, colorkey=-1)
        babus_rects = [(12, 106, 32, 48), (54, 106, 32, 48),
                       (96, 106, 32, 48), (138, 106, 32, 48)]
        babus_r = sprites.images_at(babus_rects, colorkey=-1)
        babus_rects = [(12, 170, 32, 48), (54, 170, 32, 48),
                       (96, 170, 32, 48), (138, 170, 32, 48)]
        babus_d = sprites.images_at(babus_rects, colorkey=-1)
        babus_rects = [(12, 234, 32, 48), (54, 234, 32, 48),
                       (96, 234, 32, 48), (138, 234, 32, 48)]
        babus_u = sprites.images_at(babus_rects, colorkey=-1)
        babus_rects = [(208, 192, 32, 60), (250, 192, 32, 60),
                       (292, 192, 32, 60), (334, 192, 32, 60),
                       (376, 192, 32, 60), (418, 192, 32, 60)]
        babus_jump = sprites.images_at(babus_rects, colorkey=-1)
        super().__init__([babus_l, babus_r, babus_d, babus_u, babus_jump], [
            (0, 0, 32, 48), (0, 0, 32, 48), (0, 0, 32, 48), (0, 0, 32, 48), (0, 0, 32, 60)], pos)


class Archer (Actor):
    def __init__(self, pos):
        sprites = SpriteSheet('archer.png')
        archer_rects = [(214, 18, 32, 60), (248, 18, 32, 60),
                        (214, 18, 32, 60), (180, 18, 32, 60)]
        archer_l = sprites.images_at(archer_rects, colorkey=-1)
        archer_d = [pygame.transform.flip(
            archer, True, False) for archer in archer_l]
        archer_rects = [(212, 84, 32, 60), (248, 82, 32, 60),
                        (212, 84, 32, 60), (178, 84, 32, 60)]
        archer_u = sprites.images_at(archer_rects, colorkey=-1)
        archer_r = [pygame.transform.flip(
            archer, True, False) for archer in archer_l]
        archer_rects = [(184, 286, 32, 62), (218, 286, 32, 62),
                       (252, 286, 32, 62), (218, 286, 32, 62),
                       (184, 286, 32, 62)]
        archer_jump = [pygame.transform.flip(archer, True, False) for archer in sprites.images_at(archer_rects, colorkey=-1)]
        super().__init__([archer_l, archer_r, archer_d, archer_u, archer_jump], [
            (0, 0, 32, 60), (0, 0, 32, 60), (0, 0, 32, 60), (0, 0, 32, 60), (0, 0, 32, 62)], pos)


class Templar (Actor):
    def __init__(self, pos):
        sprites = SpriteSheet('templar.png')
        templar_rects = [(12, 34, 32, 56), (54, 34, 32, 56),
                         (96, 34, 32, 56), (138, 34, 32, 56)]
        templar_l = sprites.images_at(templar_rects, colorkey=-1)
        templar_rects = [(12, 98, 32, 56), (54, 98, 32, 56),
                         (96, 98, 32, 56), (138, 98, 32, 56)]
        templar_r = sprites.images_at(templar_rects, colorkey=-1)
        templar_rects = [(12, 162, 32, 56), (54, 162, 32, 56),
                         (96, 162, 32, 56), (138, 162, 32, 56)]
        templar_d = sprites.images_at(templar_rects, colorkey=-1)
        templar_rects = [(12, 226, 32, 56), (54, 226, 32, 56),
                         (96, 226, 32, 56), (138, 226, 32, 56)]
        templar_u = sprites.images_at(templar_rects, colorkey=-1)
        templar_rects = [(478, 186, 34, 66), (520, 186, 34, 66),
                         (559, 186, 34, 66), (520, 186, 34, 66), (478, 186, 34, 66)]
        templar_jump = sprites.images_at(templar_rects, colorkey=-1)
        super().__init__([templar_l, templar_r, templar_d, templar_u, templar_jump], [
            (0, 0, 32, 56), (0, 0, 32, 56), (0, 0, 32, 56), (0, 0, 32, 56), (0, 0, 34, 66)], pos)


class Mog (Actor):
    def __init__(self, pos):
        sprites = SpriteSheet('mog.png')
        mog_rects = [(198, 152, 32, 58), (240, 152, 32, 58),
                     (198, 152, 32, 58), (156, 152, 32, 58)]
        mog_l = sprites.images_at(mog_rects, colorkey=-1)
        mog_d = [pygame.transform.flip(mog, True, False) for mog in mog_l]
        mog_rects = [(198, 56, 32, 58), (238, 56, 32, 58),
                     (198, 56, 32, 58), (158, 56, 32, 58)]
        mog_u = sprites.images_at(mog_rects, colorkey=-1)
        mog_r = [pygame.transform.flip(mog, True, False) for mog in mog_u]
        mog_rects = [(1044, 142, 32, 68), (1084, 142, 32, 68),
                       (1124, 142, 32, 68), (1084, 142, 32, 68),
                       (1044, 142, 32, 68)]
        mog_jump = [pygame.transform.flip(mog, True, False) for mog in sprites.images_at(mog_rects, colorkey=-1)]
        super().__init__([mog_l, mog_r, mog_d, mog_u, mog_jump], [
            (0, 0, 32, 56), (0, 0, 32, 56), (0, 0, 32, 56), (0, 0, 32, 56), (0, 0, 32, 68)], pos)


def find_actor(actors: list, move) -> Actor:
    for actor in actors:
        if actor.pos == (move[0], move[1]):
            return actor
    return None
