import math
import sys
import os
import pprint
import pygame
import demos.fixed_math as fixed_math

NUM_ANGLES = 256

size = width, height = 160, 144
num_tiles = width, height = size[0] // 8, size[1] // 8
center = x, y = size[0] // 2, size[1] // 2
black = 0, 0, 0
screen = None
angle = 0

math_table = []
vector_table = []
rotations = []

pygame.init()
clock = pygame.time.Clock()
screen = pygame.display.set_mode(size, pygame.SCALED | pygame.NOFRAME)
font = pygame.font.Font(pygame.font.get_default_font(), 12)


def update_fps():
    fps = str(int(clock.get_fps()))
    fps_text = font.render(fps, 1, pygame.Color("coral"))
    return fps_text


def build_math_table():
    global math_table, rotations
    for n in range(0, NUM_ANGLES, 1):
        c = math.cos(n * math.pi / (NUM_ANGLES // 2))
        s = math.sin(n * math.pi / (NUM_ANGLES // 2))
        math_table.append((c, s))

def angle_from_offset(x, y):
    v1_theta = math.atan2(0, 0)
    v2_theta = math.atan2(y, x)
    r = (v2_theta - v1_theta) * (128 / math.pi)
    if r < 0:
        r % 360
    return r


def build_vector_table():
    global vector_table, math_table
    for row in range(num_tiles[1]):
        for col in range(num_tiles[0]):
            angle = angle_from_offset(col * 8, row * 8)

            c = math.cos(angle * math.pi / 180)
            s = math.sin(angle * math.pi / 180)
            # plot = x, y = (
            #     int(round(0 + 1 * c)),
            #     int(round(0 - 1 * s)),
            # )

            plot = x, y = (
                (0 + 1 * c),
                (0 - 1 * s),
            )
            print(f"c: {col} r: {row} angle {angle} plot {plot}")
            # vector_table[row * num_tiles[0] + col] = plot
            vector_table.append(plot)




def render():
    global screen, size

    pixels = pygame.PixelArray(pygame.display.get_surface())

    for tile_row in range(0, size[1], 8):


        pygame.draw.line(screen, (255,0,0), (0, tile_row), (size[0],tile_row))

    for tile_col in range(0, size[0], 8):
        pygame.draw.line(screen, (255,0,0), (tile_col,0), (tile_col, size[1]-1))



def main_loop():

    global screen

    while 1:
        screen.fill(black)

        render()
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

def dump():
    pass

if __name__ == "__main__":
    build_math_table()
    build_vector_table()
    if len(sys.argv) == 2 and sys.argv[1] == "-dump":
        dump()
    else:
        main_loop()
