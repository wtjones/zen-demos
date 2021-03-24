#ifndef GAME_H
#define GAME_H

#include <stdbool.h>

#define GAME_KEY_UP 0
#define GAME_KEY_DOWN 1
#define GAME_KEY_LEFT 2
#define GAME_KEY_RIGHT 3
#define GAME_KEY_Q 4
#define GAME_MAX_KEYS 5
#define MAX_SNAKE_NODES 50
#define MAX_SNAKES 50

typedef enum Direction {
    Up,
    Down,
    Left,
    Right
} Direction;

typedef struct SnakeNode {
    int x;
    int y;
} SnakeNode;

typedef struct Snake {
    SnakeNode nodes[MAX_SNAKE_NODES];
    SnakeNode* head;
    int num_nodes;
    enum Direction direction;
} Snake;

extern int x, y;
extern int next_x;
extern int direction;
extern int world_max_x, world_max_y;
extern int game_keys[GAME_MAX_KEYS];
extern struct Snake snakes[MAX_SNAKES];
extern int num_snakes;

extern void game_init(int, int);
bool game_update(int max_x, int max_y);

#endif
