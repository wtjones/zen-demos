#ifndef GAME_H
#define GAME_H

#include "world.h"
#include <stdbool.h>

#define GAME_KEY_UP 0
#define GAME_KEY_DOWN 1
#define GAME_KEY_LEFT 2
#define GAME_KEY_RIGHT 3
#define GAME_KEY_Q 4
#define GAME_MAX_KEYS 5
#define MAX_SNAKE_NODES 1
#define MAX_SNAKES 8
#define MAX_WALLS 300 * 200 // redefined because clang-format issue

typedef enum Direction {
    UP,
    DOWN,
    LEFT,
    RIGHT
} Direction;

typedef struct Wall {
    WorldEntity world_entity;
} Wall;

typedef struct Snake {
    WorldEntity nodes[MAX_SNAKE_NODES];
    WorldEntity* head;
    int num_nodes;
    enum Direction direction;
} Snake;

extern int game_keys[GAME_MAX_KEYS];
extern Snake snakes[MAX_SNAKES];
extern Wall walls[MAX_WALLS];
extern int num_snakes;
extern int num_walls;

extern void game_init(int, int);
void add_walls();
void wall_init();
bool game_update(int max_x, int max_y);

#endif
