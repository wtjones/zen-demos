import math
import sys
import os
import numpy as np
import pygame
from pathlib import Path

map_image = os.path.join(Path.home(), "Dropbox/finalmap_0.png")
size = width, height = 40, 32
print(size)
center = x, y = size[0] // 2, size[1] // 2
black = 0, 0, 0
screen = None
zeors_array = np.zeros((2, 3))
print(zeors_array)
map_surface = None
angle = 0
viewer = x, y = center[0], center[1]
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
        rotations.append(np.array([[c, -s], [s, c]]))


def render_map():
    global screen, map_surface, size, viewer, viewer_angle, world

    # get top-left starting coord of world map
    start = x, y = viewer[0] - center[0], viewer[1] - center[1]
    for scanline_y in range(size[1]):
        for scanline_x in range(size[0]):

            peek = x, y = start[0] + scanline_x, start[1] + scanline_y

            # translate to origin
            p = np.array([[peek[0] - viewer[0]], [peek[1] - viewer[1]]])
            rotated = rotations[viewer_angle].dot(p)
            # translate back
            peek = int(rotated[0] + viewer[0]), int(rotated[1] + viewer[1])

            if (
                peek[0] >= 0
                and peek[0] < world[0]
                and peek[1] >= 0
                and peek[1] < world[1]
            ):
                color = map_surface.get_at(peek)
            else:
                color = (0, 0, 0)
            screen.set_at((scanline_x, scanline_y), color)


def render_viz():
    global angle
    p = np.array([[20], [10]])

    rotated = rotations[angle].dot(p)
    plot = x, y = int(rotated[0][0] + center[0]), int(rotated[1][0] + center[1])
    print(plot)
    screen.set_at((plot), (20, 100, 50))
    pygame.draw.circle(screen, 3000, plot, 5)

    angle += 1
    if angle > 359:
        angle = 0


def main_loop():

    global screen, map_surface, viewer, world

    map_surface = pygame.image.load(map_image).convert()
    world = x, y = map_surface.get_width(), map_surface.get_height()
    viewer = x, y = world[0] // 2, world[1] // 2

    while 1:
        screen.fill(black)

        render_map()
        screen.blit(update_fps(), (2, 0))
        pygame.display.flip()
        clock.tick(60)
        handle_input()


def handle_input():
    global viewer, viewer_angle, scroll_speed
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        if event.type == pygame.KEYDOWN:
            if event.key == pygame.K_RETURN or event.key == pygame.K_ESCAPE:
                sys.exit()
    key = pygame.key.get_pressed()

    if key[pygame.K_LEFT]:
        viewer = viewer[0] - scroll_speed, viewer[1]
    if key[pygame.K_RIGHT]:
        viewer = viewer[0] + scroll_speed, viewer[1]
    if key[pygame.K_UP]:
        viewer = viewer[0], viewer[1] - scroll_speed
    if key[pygame.K_DOWN]:
        viewer = viewer[0], viewer[1] + scroll_speed

    viewer_angle += 1
    if viewer_angle > 359:
        viewer_angle = 0


if __name__ == "__main__":
    build_tables()
    main_loop()
