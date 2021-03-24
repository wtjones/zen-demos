#include "game.h"
#include <stdlib.h>

int world_max_x, world_max_y;
int game_keys[GAME_MAX_KEYS];
struct Snake snakes[MAX_SNAKES];
int num_snakes;

void game_init(int max_x, int max_y)
{
    world_max_x = max_x;
    world_max_y = max_y;

    num_snakes = max_x / 5;

    for (size_t i = 0; i < num_snakes; i++) {
        struct Snake* snake = &snakes[i];
        snake->head = &snake->nodes[0];
        snake->direction = rand() % 4;
        snake->head->x = rand() % world_max_y;
        snake->head->y = rand() % world_max_x;
        snake->num_nodes = 1;
    }
}

Direction get_random_turn(Direction direction)
{
    return (direction == Up || direction == Down)
        ? rand() % 2 == 0 ? Left
                          : Right
        : rand() % 2 == 0 ? Up
                          : Down;
}

bool game_update(int max_x, int max_y)
{
    if (game_keys[GAME_KEY_Q] == true) {
        return true;
    }
    world_max_x = max_x;
    world_max_y = max_y;

    for (size_t i = 0; i < num_snakes; i++) {
        Snake* snake = &snakes[i];
        Direction next_direction = snake->direction;

        int avoidance_distance = (rand() % (world_max_x / 4)) + 2;
        if (snake->direction == Up && snake->head->y < avoidance_distance) {
            next_direction = get_random_turn(snake->direction);
        }

        if (snake->direction == Down && snake->head->y > world_max_y - avoidance_distance) {
            next_direction = get_random_turn(snake->direction);
        }

        if (snake->direction == Left && snake->head->x < avoidance_distance) {
            next_direction = get_random_turn(snake->direction);
        }

        if (snake->direction == Right && snake->head->x > world_max_x - avoidance_distance) {
            next_direction = get_random_turn(snake->direction);
        }

        // if we haven't turned, maybe turn anyway
        if (snake->direction == next_direction && rand() % 20 == 1) {
            next_direction = get_random_turn(snake->direction);
        }

        snake->direction = next_direction;

        snake->head->y -= snake->direction == Up ? 1 : 0;
        snake->head->y += snake->direction == Down ? 1 : 0;
        snake->head->x -= snake->direction == Left ? 1 : 0;
        snake->head->x += snake->direction == Right ? 1 : 0;
    }
    return false;
}
