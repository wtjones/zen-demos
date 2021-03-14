#include "game.h"

int x = 0, y = 0;
int next_x = 0;
int direction = 1;
int world_max_x, world_max_y;
int game_keys[4];

void game_init(int max_x, int max_y)
{
    y = max_y / 2;
    world_max_x = max_x;
    world_max_y = max_y;
}

void game_update(int max_x, int max_y)
{
    world_max_x = max_x;
    world_max_y = max_y;

    y -= game_keys[GAME_KEY_UP] == 1 ? 1 : 0;
    y += game_keys[GAME_KEY_DOWN] == 1 ? 1 : 0;

    next_x = x + direction;

    if (next_x >= world_max_x || next_x < 0) {
        direction *= -1;
    } else {
        x += direction;
    }
}
