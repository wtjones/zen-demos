#ifndef GAME_H
#define GAME_H

#include <stdbool.h>

extern int x, y;
extern int next_x;
extern int direction;
extern int world_max_x, world_max_y;

void game_init(int, int);
void game_update(int max_x, int max_y, bool key_up, bool key_down);

#endif
