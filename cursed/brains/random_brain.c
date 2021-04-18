#include "random_brain.h"
#include <stdlib.h>

Direction get_random_turn(Direction direction)
{
    return (direction == UP || direction == DOWN)
        ? rand() % 2 == 0 ? LEFT
                          : RIGHT
        : rand() % 2 == 0 ? UP
                          : DOWN;
}

Direction run_random_brain(Snake* snake)
{
    Direction next_direction = snake->direction;
    int avoidance_distance = (rand() % (world_max_x / 4)) + 2;
    if (snake->direction == UP && snake->head->y < avoidance_distance) {
        next_direction = get_random_turn(snake->direction);
    }

    if (snake->direction == DOWN && snake->head->y > world_max_y - avoidance_distance) {
        next_direction = get_random_turn(snake->direction);
    }

    if (snake->direction == LEFT && snake->head->x < avoidance_distance) {
        next_direction = get_random_turn(snake->direction);
    }

    if (snake->direction == RIGHT && snake->head->x > world_max_x - avoidance_distance) {
        next_direction = get_random_turn(snake->direction);
    }

    // if we haven't turned, maybe turn anyway
    if (snake->direction == next_direction && rand() % 20 == 1) {
        next_direction = get_random_turn(snake->direction);
    }
    return next_direction;
}
