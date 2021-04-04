#ifndef DRAW_H
#define DRAW_H

#include "util.h"

#define CHECK "\xe2\x9c\x93"
#define WALL "\u25A3"
#define SNAKE_HEAD "@"
#define SNAKE_BODY "+"
#define PELLET "o"

extern int g_max_y, g_max_x;

void draw_init();
void draw_cleanup();
void draw_wall();
void draw();

#endif
