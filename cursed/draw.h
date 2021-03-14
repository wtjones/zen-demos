#ifndef DRAW_H
#define DRAW_H

#define CHECK "\xe2\x9c\x93"
#define WALL "\u25A3"

extern int g_max_y, g_max_x;

void draw_init();

void draw_wall();

void draw();

#endif
