#include "alpha_brain.h"
#include <assert.h>
#include <stdlib.h>

#define SEEK_ERROR 100000
#define COLLIDE_THRESHOLD 1

int find_entity(int start_x, int start_y, Direction direction, WorldEntity** found)
{
    int distance = SEEK_ERROR;
    int count = 0;
    bool done = false;
    int seek_x = start_x, seek_y = start_y;
    *found = NULL;

    while (done == false) {
        seek_y -= direction == UP ? 1 : 0;
        seek_y += direction == DOWN ? 1 : 0;
        seek_x -= direction == LEFT ? 1 : 0;
        seek_x += direction == RIGHT ? 1 : 0;
        count++;

        if (!in_world_bounds(seek_x, seek_y)) {
            done = true;
            distance = count;
        } else {
            WorldEntity* entity = get_entity_at(seek_x, seek_y);
            if (entity != NULL) {
                done = true;
                distance = count;
                *found = entity;
            }
        }
    }
    return distance;
}

/**
 * get nearest pellet direction
 * get furthest open path direction
 * get forward distance to obstacle
 * if pellet is an option, use direction
 * else if forward is near an obstacle, choose most open direction
 * else continue current direction
 */
Direction run_alpha_brain(Snake* snake)
{

    int distance = SEEK_ERROR;
    int pellet_distance = SEEK_ERROR;
    Direction pellet_direction;
    int forward_distance = SEEK_ERROR;
    int open_distance = 0;
    Direction open_direction;
    Direction direction = 0;
    Direction next_direction = snake->direction;

    log_debug("alpha brain: current direction %d", snake->direction);
    for (size_t i = 0; i < 4; i++) {
        direction = (Direction)i;
        if (snake->direction != (i + 2) % 4) {
            WorldEntity* entity = NULL;
            int seek_dist = find_entity(snake->head->x, snake->head->y, direction, &entity);
            log_debug("alpha brain: find_entity of direction %d result entity %d dist %d",
                direction,
                entity != NULL ? entity->owner_type : -1,
                seek_dist);

            forward_distance = direction == snake->direction
                ? seek_dist
                : forward_distance;

            log_debug("alpha brain: seeking direction %d", direction);

            if (entity != NULL && entity->owner_type == PELLET_ENTITY) {
                if (seek_dist < pellet_distance) {
                    pellet_distance = seek_dist;
                    pellet_direction = direction;
                    log_debug("alpha brain: found pellet at dist %d", seek_dist);
                }
            }

            // non-pellet obstacle or boundary
            if (entity == NULL || (entity->owner_type != PELLET_ENTITY)) {
                if (seek_dist > open_distance) {
                    open_distance = seek_dist;
                    open_direction = direction;
                    log_debug("alpha brain: found open dist of %d", seek_dist);
                }
            }
        }
    }
    log_debug("alpha brain: forward_dist of %d", forward_distance);

    if (pellet_distance != SEEK_ERROR) {
        next_direction = pellet_direction;
        log_debug("alpha brain: choosing pellet");
    } else if (forward_distance <= COLLIDE_THRESHOLD) {
        next_direction = open_direction;
        log_debug("alpha brain: choosing open direction");
    }

    log_debug("Orig direction %d new direction %d distance %d",
        snake->direction, next_direction, distance);
    return next_direction;
}
