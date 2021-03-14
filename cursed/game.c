#include "game.h"

int x = 0, y = 0;
int next_x = 0;
int direction = 1;
int world_max_x, world_max_y;

void game_init(int max_x, int max_y)
{
    y = max_y / 2;
    world_max_x = max_x;
    world_max_y = max_y;
}

void game_update(int max_x, int max_y, bool key_up, bool key_down)
{
    world_max_x = max_x;
    world_max_y = max_y;

    y -= key_up == 1 ? 1 : 0;
    y += key_down == 1 ? 1 : 0;

    next_x = x + direction;

    if (next_x >= world_max_x || next_x < 0) {
        direction *= -1;
    } else {
        x += direction;
    }
}
