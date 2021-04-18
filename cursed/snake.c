#include "snake.h"
#include "game.h"
#include <assert.h>
#include <stdlib.h>

void snake_update(Snake* snake)
{
    snake->direction = snake->brain(snake);

    int current_x, new_x = current_x = snake->head->x;
    int current_y, new_y = current_y = snake->head->y;

    new_y -= snake->direction == UP ? 1 : 0;
    new_y += snake->direction == DOWN ? 1 : 0;
    new_x -= snake->direction == LEFT ? 1 : 0;
    new_x += snake->direction == RIGHT ? 1 : 0;

    bool in_bounds = new_x > 1 && new_x < world_max_x - 1 && new_y > 1 && new_y < world_max_y - 1;
    if (in_bounds) {
        WorldNode* target_node = get_world_node(new_x, new_y);

        // are we crashing into a snake?
        bool crashing = entity_type_exists(
            target_node->world_entity, SNAKE_ENTITY);

        if (crashing) {
            log_trace(
                "Collision: cur %d %d new %d %d",
                current_x, current_y, new_x, new_y);
            for (size_t j = 0; j < snake->num_nodes; j++) {
                WorldEntity* node = &snake->nodes[j];
                clear_entity(node);
            }
            snake->num_nodes = 0;
            snake->head = NULL;
        } else {

            // are we moving into a pellet?
            bool eating
                = entity_type_exists(
                    target_node->world_entity, PELLET_ENTITY);

            if (eating) {
                log_trace("Removing pellet at x: %d, y: %d", new_x, new_y);
                clear_entity(target_node->world_entity);
                log_trace("Removed pellet at x: %d, y: %d", new_x, new_y);
                num_pellets--;
            }
            int new_node_x = new_x;
            int new_node_y = new_y;
            int last_node_x, last_node_y;

            for (size_t j = 0; j < snake->num_nodes; j++) {
                assert(j < 100);
                WorldEntity* node = &snake->nodes[j];
                last_node_x = node->x;
                last_node_y = node->y;
                move_to(new_node_x, new_node_y, node);
                new_node_x = last_node_x;
                new_node_y = last_node_y;
            }

            // if we have a pellet, consume to add a tail node
            if (snake->num_pellets > 0 && snake->num_nodes < MAX_SNAKE_NODES) {
                WorldEntity* node = &snake->nodes[snake->num_nodes++];
                clear_entity(node);
                node->owner_type = SNAKE_ENTITY;
                node->owner = snake;
                spawn_at(current_x, current_y, node);
                snake->num_pellets--;
            }
            if (eating) {
                snake->num_pellets++;
            }
        }
    }
}

void snakes_update()
{
    for (size_t i = 0; i < MAX_SNAKES; i++) {
        Snake* snake = &snakes[i];
        if (entity_exists(snake->head)) {
            snake_update(snake);
        }
    }
}