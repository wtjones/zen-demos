#include "game.h"

int world_max_x, world_max_y;
int game_keys[4];
struct Snake snake;

void game_init(int max_x, int max_y)
{
    world_max_x = max_x;
    world_max_y = max_y;
    snake.direction = Down;
    snake.nodes[0].y = max_y / 2;
    snake.nodes[0].x = world_max_x / 2;
    snake.num_nodes = 1;
}

void game_update(int max_x, int max_y)
{
    world_max_x = max_x;
    world_max_y = max_y;

    if (game_keys[GAME_KEY_UP] == 1) {
        snake.direction = Up;
    }
    if (game_keys[GAME_KEY_DOWN] == 1) {
        snake.direction = Down;
    }
    if (game_keys[GAME_KEY_LEFT] == 1) {
        snake.direction = Left;
    }
    if (game_keys[GAME_KEY_RIGHT] == 1) {
        snake.direction = Right;
    }

    SnakeNode* head = snake.nodes;

    head->y -= snake.direction == Up ? 1 : 0;
    head->y += snake.direction == Down ? 1 : 0;
    head->x -= snake.direction == Left ? 1 : 0;
    head->x += snake.direction == Right ? 1 : 0;
}
