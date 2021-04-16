#ifndef GAME_H
#define GAME_H

#include "entity.h"
#include "util.h"
#include "world.h"
#include <stdbool.h>

#define GAME_KEY_UP 0
#define GAME_KEY_DOWN 1
#define GAME_KEY_LEFT 2
#define GAME_KEY_RIGHT 3
#define GAME_KEY_Q 4
#define GAME_MAX_KEYS 5
#define MAX_SNAKES 8
#define MAX_WALLS 300 * 200 // redefined because clang-format issue
#define MAX_PELLETS 50
#define PELLET_RATE 3
#define PELLET_DECAY 60

extern int game_keys[GAME_MAX_KEYS];
extern Snake snakes[MAX_SNAKES];
extern Wall walls[MAX_WALLS];
extern Pellet pellets[MAX_PELLETS];
extern int num_snakes;
extern int num_walls;
extern int num_pellets;

void game_init(int, int);
void game_cleanup();
void add_walls();
void wall_init();
bool game_update(int max_x, int max_y);
void snake_update(Snake* snake);
void snakes_update();

#endif
