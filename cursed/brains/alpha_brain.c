#include "alpha_brain.h"
#include <assert.h>
#include <stdlib.h>

#define SEEK_ERROR 100000

int find_pellet(int x, int y, Direction direction)
{
    int distance = SEEK_ERROR;
    int count = 0;
    bool done = false;
    int seek_x = x, seek_y = y;

    while (done == false) {
        seek_y -= direction == UP ? 1 : 0;
        seek_y += direction == DOWN ? 1 : 0;
        seek_x -= direction == LEFT ? 1 : 0;
        seek_x += direction == RIGHT ? 1 : 0;
        count++;

        if (!in_world_bounds(seek_x, seek_y)) {
            done = true;
        } else {
            WorldEntity* entity = get_entity_at(seek_x, seek_y);

            if (entity != NULL) {
                if (entity->owner_type == PELLET_ENTITY) {
                    done = true;
                    distance = count;
                } else {
                    // not a pellet, so search fails
                    done = true;
                }
            }
        }
    }
    return distance;
}

Direction run_alpha_brain(Snake* snake)
{

    Direction nearest_dir = snake->direction;
    int distance = SEEK_ERROR;

    if (snake->direction != DOWN) {
        int seek_dist = find_pellet(snake->head->x, snake->head->y, UP);
        distance = seek_dist < distance ? seek_dist : distance;
        nearest_dir = distance == seek_dist && distance != SEEK_ERROR
            ? UP
            : nearest_dir;
    }

    if (snake->direction != LEFT) {
        int seek_dist = find_pellet(snake->head->x, snake->head->y, RIGHT);
        distance = seek_dist < distance ? seek_dist : distance;
        nearest_dir = distance == seek_dist && distance != SEEK_ERROR
            ? RIGHT
            : nearest_dir;
    }

    if (snake->direction != UP) {
        int seek_dist = find_pellet(snake->head->x, snake->head->y, DOWN);
        distance = seek_dist < distance ? seek_dist : distance;
        nearest_dir = distance == seek_dist && distance != SEEK_ERROR
            ? DOWN
            : nearest_dir;
    }

    if (snake->direction != RIGHT) {
        int seek_dist = find_pellet(snake->head->x, snake->head->y, LEFT);
        distance = seek_dist < distance ? seek_dist : distance;
        nearest_dir = distance == seek_dist && distance != SEEK_ERROR
            ? LEFT
            : nearest_dir;
    }

    Direction next_direction = nearest_dir;
    log_debug("Orig direction %d new direction %d distance %d",
        snake->direction, next_direction, distance);
    return next_direction;
}
