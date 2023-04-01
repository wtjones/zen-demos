import math
import sys
import random
import pprint
import pygame
import demos.fixed_math as fixed_math

pygame.init()

clock = pygame.time.Clock()

size = width, height = 160, 144
print(size)
center = x, y = 80, 140
black = 0, 0, 0
screen = None
math_table = []


def build_table():
    for n in range(0, 360, 1):
        c = math.cos(n * math.pi / 180)
        s = math.sin(n * math.pi / 180)
        print(f"angle: {n}")
        item = c, s = fixed_math.float_to_fixed_8_8(c), fixed_math.float_to_fixed_8_8(s)
        math_table.append(item)


def viz():
    screen = pygame.display.set_mode(size, pygame.SCALED)

    ball = x, y = 0, 80

    while 1:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                sys.exit()
            if event.type == pygame.KEYDOWN or event.type == pygame.KEYUP:
                sys.exit()

        screen.fill(black)

        pygame.draw.circle(screen, 400, ball, 20)
        x = ball[0]
        y = size[1] / 2
        normal_x = x / size[0] * 10
        y += math.sin(normal_x) * size[1] / 4

        print(f"sin x {math.sin(x)}, normal: {normal_x}, y: {y}")
        ball = x, y = ball[0] + 1, y
        if ball[0] > size[0]:
            ball = x, y = math.sin(size[0] / ball[0]), ball[1]

        draw_fan(screen, (0, 140))
        draw_fan_fixed(screen, (80, 140))

        pygame.display.flip()
        clock.tick(60)


def draw_fan(screen, origin):
    max_dist = 120
    for n in range(45, 90, 3):
        c = math.cos(n * math.pi / 180)
        s = math.sin(n * math.pi / 180)

        for dist in range(1, max_dist):
            plot = x, y = (
                int(round(origin[0] + dist * c)),
                int(round(origin[1] - dist * s)),
            )
            color = (int(n / 2), dist + 50, dist + 75)
            screen.set_at((plot), color)


def draw_fan_fixed(screen, origin):

    max_dist = 120
    for n in range(45, 90, 3):

        c = math_table[n][0]
        s = math_table[n][1]
        print(f"got math: {c} {s}")
        for dist in range(1, max_dist):

            x_offset = (dist * c) >> 8
            y_offset = (dist * s) >> 8
            print(f"got math offset: {x_offset} {y_offset}")

            plot = x, y = origin[0] + x_offset, origin[1] - y_offset

            color = (int(n / 3), dist + 50, dist + 75)
            screen.set_at((plot), color)


def build():
    for n in range(0, 360, 1):
        c = math.cos(n * math.pi / 180)
        s = math.sin(n * math.pi / 180)
        math_table.append((c, s))
        print("{0:b}".format(c))


if __name__ == "__main__":
    build_table()
    pprint.pprint(math_table)
    viz()
