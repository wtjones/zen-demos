import math
import sys
import os
import pygame
from pathlib import Path
import demos.fixed_math as fixed_math

map_image = os.path.join(Path.home(), "Dropbox/finalmap_0.png")
size = width, height = 160, 144
fov = size[0]
# Number of pixels scanline 0 is below the horizon. Since 0 is at the top,
# the value will generally be negative.
min_horizon = size[1] // -2 + 2
horizon = min_horizon
center = x, y = size[0] // 2, size[1] // 2
black = 0, 0, 0
screen = None
map_surface = None
angle = 0
viewer = x, y, z = 0, 0, 0
viewer_angle = 55
world = None
scroll_speed = 2
math_table = []
rotations = []

pygame.init()
clock = pygame.time.Clock()
screen = pygame.display.set_mode(size, pygame.SCALED)
font = pygame.font.Font(pygame.font.get_default_font(), 8)


def update_fps():
    fps = str(int(clock.get_fps()))
    fps_text = font.render(fps, 1, pygame.Color("coral"))
    return fps_text


def build_math_table():
    global math_table, rotations
    for n in range(0, 360, 1):
        c = math.cos(n * math.pi / 180)
        s = math.sin(n * math.pi / 180)
        math_table.append((c, s))


def build_rotations_table():
    """Create a table by angle of:
        - rotation relative to top left pixel
        - delta to next pixel
    This approach does not support scaling currently.
    """
    global math_table, rotations, viewer
    start = x, y = viewer[0] - center[0], viewer[1] - center[1]
    scanline_y = 0

    scanline_x = 0
    peek = x, y = start[0] + scanline_x, start[1] + scanline_y
    # translate to origin
    p = (peek[0] - viewer[0], peek[1] - viewer[1])

    for n in range(0, 360, 1):
        c = math_table[n][0]
        s = math_table[n][1]

        rotated = c * p[0] - s * p[1], c * p[1] + s * p[0]

        # rotate the adjacent pixel for the delta
        rotated2 = c * (p[0] + 1) - s * p[1], c * p[1] + s * (p[0] + 1)

        line_dx = rotated2[0] - rotated[0]
        line_dy = rotated2[1] - rotated[1]

        rotated_fixed = (
            fixed_math.float_to_fixed_8_8(rotated[0]),
            fixed_math.float_to_fixed_8_8(rotated[1]),
        )

        rotations.append(
            (
                rotated_fixed,
                (
                    fixed_math.float_to_fixed_8_8(line_dx),
                    fixed_math.float_to_fixed_8_8(line_dy),
                ),
            )
        )


def render_map():
    global screen, map_surface, size, viewer, viewer_angle, world, rotations

    rotation = rotations[viewer_angle]
    rotated = rotation[0]
    line_dx = rotation[1][0]
    line_dy = rotation[1][1]

    # translate back
    peek = (
        rotated[0] + fixed_math.int_8_to_fixed_8_8(viewer[0]),
        rotated[1] + fixed_math.int_8_to_fixed_8_8(viewer[1]),
    )

    pixels = pygame.PixelArray(pygame.display.get_surface())
    map_pixels = pygame.PixelArray(map_surface)

    for scanline_y in range(size[1]):

        peek_row = peek
        for scanline_x in range(size[0]):
            peek_map = (
                fixed_math.fixed_8_8_to_int_8(peek_row[0]),
                fixed_math.fixed_8_8_to_int_8(peek_row[1]),
            )

            if (
                peek_map[0] >= 0
                and peek_map[0] < world[0]
                and peek_map[1] >= 0
                and peek_map[1] < world[1]
            ):
                color = map_pixels[peek_map[0], peek_map[1]]
            else:
                color = (0, 0, 0)
            pixels[scanline_x, scanline_y] = color
            peek_row = (peek_row[0] + line_dx, peek_row[1] + line_dy)

        # move down a row
        peek = (peek[0] + -line_dy, peek[1] + line_dx)


def main_loop():

    global screen, map_surface, viewer, world

    map_surface = pygame.image.load(map_image).convert()
    world = x, y = map_surface.get_width(), map_surface.get_height()
    viewer = x, y, z = world[0] // 2, world[1] // 2, 10

    while 1:
        screen.fill(black)

        render_map()
        screen.blit(update_fps(), (2, 0))
        pygame.display.flip()
        clock.tick(60)
        handle_input()


def handle_input():
    global viewer, viewer_angle, scroll_speed, fov, horizon
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        if event.type == pygame.KEYDOWN:
            if event.key == pygame.K_RETURN or event.key == pygame.K_ESCAPE:
                sys.exit()
    key = pygame.key.get_pressed()

    if key[pygame.K_LEFT]:
        viewer = viewer[0] - scroll_speed, viewer[1], viewer[2]
    if key[pygame.K_RIGHT]:
        viewer = viewer[0] + scroll_speed, viewer[1], viewer[2]
    if key[pygame.K_UP]:
        viewer = viewer[0], viewer[1] - scroll_speed, viewer[2]
    if key[pygame.K_DOWN]:
        viewer = viewer[0], viewer[1] + scroll_speed, viewer[2]
    if key[pygame.K_a]:
        viewer = viewer[0], viewer[1], viewer[2] + 1
    if key[pygame.K_z]:
        viewer = viewer[0], viewer[1], viewer[2] - 1

    viewer_angle += 1
    if viewer_angle > 359:
        viewer_angle = 0


if __name__ == "__main__":
    build_math_table()
    build_rotations_table()
    main_loop()
