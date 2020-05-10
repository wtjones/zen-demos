import math
import sys
import random
import pprint
import pygame
pygame.init()

clock = pygame.time.Clock()

size = width, height = 160, 144
print(size)
center = x, y = 80, 140
black = 0, 0, 0
screen = None
math_table = []
print(math.pi)

# for n in range(0, 360):
#     print(math.cos(n * math.pi / 180))
#     print(math.sin(n * math.pi / 180))


def build_table():
    for n in range(0, 360, 1):
        c = math.cos(n * math.pi / 180)
        s = math.sin(n * math.pi / 180)
        print(f"angle: {n}")
        item = c, s = float_to_fixed_8_8(c), float_to_fixed_8_8(s)
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
        #pygame.draw.circle(screen, 400, (0, 0), 20)

        pygame.draw.circle(screen, 400, ball, 20)
        x = ball[0]
        y = size[1] / 2
        normal_x = x / size[0] * 10
        y += (math.sin(normal_x) * size[1] / 4)

        print(f"sin x {math.sin(x)}, normal: {normal_x}, y: {y}")
        ball = x, y = ball[0] + 1, y
        if ball[0] > size[0]:
            ball = x, y = math.sin(size[0] / ball[0]), ball[1]

        draw_fan(screen, (0, 140))
        draw_fan_fixed(screen, (80, 140))
        # max_dist = 100
        # for n in range(45, 90, 3):
        #     c = math.cos(n * math.pi / 180)
        #     s = math.sin(n * math.pi / 180)

        #     for dist in range(1, max_dist):
        #         plot = x, y = int(round(origin[0] + dist * c)), \
        #             int(round(origin[1] - dist * s))
        #         color = (int(n / 2), dist + 50, dist + 75)
        #         screen.set_at((plot), color)

        pygame.display.flip()
        clock.tick(60)


# def viz_fixed(origin):
#     screen = pygame.display.set_mode(size, pygame.SCALED)

#     ball = x, y = 0, 80

#     while 1:
#         for event in pygame.event.get():
#             if event.type == pygame.QUIT:
#                 sys.exit()
#             if event.type == pygame.KEYDOWN or event.type == pygame.KEYUP:
#                 sys.exit()

#         screen.fill(black)

#         max_dist = 100
#         for n in range(45, 90, 3):

#             c = math_table[n][0]
#             s = math_table[n][1]
#             print(f"got math: {c} {s}")
#             for dist in range(1, max_dist):

#                 x_offset = (dist * c) >> 8
#                 y_offset = (dist * s) >> 8
#                 print(f"got math offset: {x_offset} {y_offset}")

#                 plot = x, y = origin[0] + x_offset, origin[1] - y_offset

#                 color = (int(n / 3), dist + 50, dist + 75)
#                 screen.set_at((plot), color)

#         pygame.display.flip()
#         clock.tick(60)


def draw_fan(screen, origin):
    max_dist = 120
    for n in range(45, 90, 3):
        c = math.cos(n * math.pi / 180)
        s = math.sin(n * math.pi / 180)

        for dist in range(1, max_dist):
            plot = x, y = int(round(origin[0] + dist * c)), \
                int(round(origin[1] - dist * s))
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


def fixed_8_8_to_8(n):
    return n >> 8


def float_to_fixed_8_8(n):
    whole = int(n)
    frac = n - whole
    fixed_frac = int(frac * 256)
    fixed_8_8 = whole * 256 + fixed_frac
    print(f"original: {n}")
    print("fixed_frac: {0:b}".format(fixed_frac))
    print("fixed 8.8: {0:016b}".format(fixed_8_8))
    return fixed_8_8


def multiply_8_by_fixed_8_8(int_8, fixed_8_8):
    f = int_8
    print("first param fixed 8.8: {0:016b}".format(f))
    print("secon param fixed 8.8: {0:016b}".format(fixed_8_8))
    return fixed_8_8 * f


if __name__ == "__main__":
    # build()
    f = float_to_fixed_8_8(1.3)
    #f = float_to_fixed_8_8(3.9)
    m = multiply_8_by_fixed_8_8(10, f)
    print("result fixed: {0:016b}".format(m))
    print("result shift: {0:016b}".format(m >> 8))

    print(f"result: {m >> 8}")
    build_table()
    pprint.pprint(math_table)
    viz()

    # 0000 0011 1000 0000
    # 0000 0010 0000 0000

    # 0000001110000000
    # print("{0:b}".format(c))
