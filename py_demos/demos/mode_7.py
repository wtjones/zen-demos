import math
import sys
import os
import pygame
from pathlib import Path

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


def build_tables():
    global math_table, rotations
    for n in range(0, 360, 1):
        c = math.cos(n * math.pi / 180)
        s = math.sin(n * math.pi / 180)
        math_table.append((c, s))


def render_map_3d():
    global screen, map_surface, size, viewer, viewer_angle, world, fov, horizon

    scale_y = 200
    scale_x = 200

    space_x = 0
    space_y = 0
    space_z = viewer[2]
    angle = viewer_angle

    for scanline_y in range(size[1] // 2, size[1]):

        distance = space_z * scale_y / (scanline_y + horizon)
        horizonal_scale = distance / scale_x

        s = math_table[angle][0]
        c = math_table[angle][1]

        line_dx = -s * horizonal_scale
        line_dy = c * horizonal_scale

        space_x = viewer[0] + distance * c - size[0] // 2 * line_dx
        space_y = viewer[1] + distance * s - size[1] // 2 * line_dy

        for scanline_x in range(size[0]):
            peek = int(space_x), int(space_y)
            if (
                peek[0] >= 0
                and peek[0] < world[0]
                and peek[1] >= 0
                and peek[1] < world[1]
            ):
                color = map_surface.get_at(peek)
            else:
                color = (200, 0, 0)

            screen.set_at((scanline_x, scanline_y), color)
            space_x += line_dx
            space_y += line_dy


def main_loop():

    global screen, map_surface, viewer, world

    map_surface = pygame.image.load(map_image).convert()
    world = x, y = map_surface.get_width(), map_surface.get_height()
    viewer = x, y, z = world[0] // 2, world[1] // 2, 10

    while 1:
        screen.fill(black)

        render_map_3d()
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
    if key[pygame.K_f]:
        fov += 1
        print(f"FOV: {fov}")
    if key[pygame.K_g]:
        fov -= 1
        print(f"FOV: {fov}")
    if key[pygame.K_h]:
        horizon += 1
        print(f"Horiz: {horizon}")
    if key[pygame.K_j]:
        horizon = horizon - 1 if horizon - 1 >= min_horizon else min_horizon
        print(f"Horiz: {horizon}")
    if key[pygame.K_a]:
        viewer = viewer[0], viewer[1], viewer[2] + 1
    if key[pygame.K_z]:
        viewer = viewer[0], viewer[1], viewer[2] - 1

    viewer_angle += 1
    if viewer_angle > 359:
        viewer_angle = 0


if __name__ == "__main__":
    build_tables()
    main_loop()
