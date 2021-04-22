#ifndef DRAW_H
#define DRAW_H

#include "util.h"

#define CHECK "\xe2\x9c\x93"
#define WALL "\u25A3"
#define SNAKE_HEAD "@"
#define SNAKE_BODY "+"
#define PELLET "o"
#define WALL_COLOR 1
#define SNAKE_COLOR 2
#define PELLET_COLOR 3
#define RANDOM_BRAIN_COLOR 4
#define ALPHA_BRAIN_COLOR 5
#define TEXT_COLOR 6

extern int g_max_y, g_max_x;

void draw_init();
void draw_cleanup();
void draw_wall();
void draw();

#endif
