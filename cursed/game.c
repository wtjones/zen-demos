#include "game.h"
#include <assert.h>
#include <stdlib.h>

int game_keys[GAME_MAX_KEYS];
Snake snakes[MAX_SNAKES];
Wall walls[MAX_WALLS];
Pellet pellets[MAX_PELLETS];
int num_snakes = 0;
int num_walls = 0;
int num_pellets = 0;
int game_iteration = 0;

void game_init(int max_x, int max_y)
{
    world_init(max_x, max_y);
    add_walls();

    num_snakes = max_x / 5;
    for (size_t i = 0; i < num_snakes; i++) {
        Snake* snake = &snakes[i];
        snake->head = &snake->nodes[0];
        snake->head->owner_type = SNAKE_ENTITY;
        snake->head->owner = snake;

        snake->direction = rand() % 4;
        move_to(
            (rand() % (world_max_x - 2)) + 1,
            (rand() % (world_max_y - 2)) + 1,
            snake->head);
        snake->num_nodes = 1;
    }
}
void game_cleanup()
{
    world_cleanup();
}
void add_walls()
{
    Wall* wall;
    num_walls = 0;
    assert(world_max_x * world_max_y <= MAX_WALLS);
    assert(world_max_x > 1);
    for (size_t i = 0; i < world_max_y; i++) {
        wall = &walls[num_walls++];
        wall_init(wall, 0, i);
        wall = &walls[num_walls++];
        wall_init(wall, world_max_x, i);
    }

    for (size_t i = 1; i < world_max_x; i++) {
        wall = &walls[num_walls++];
        wall_init(wall, i, 0);
        wall = &walls[num_walls++];
        wall_init(wall, i, world_max_y - 1);
    }
}

void wall_init(Wall* wall, int x, int y)
{
    wall->world_entity.owner_type = WALL_ENTITY;
    wall->world_entity.owner = wall;
    move_to(x, y, &wall->world_entity);
}

Direction get_random_turn(Direction direction)
{
    return (direction == UP || direction == DOWN)
        ? rand() % 2 == 0 ? LEFT
                          : RIGHT
        : rand() % 2 == 0 ? UP
                          : DOWN;
}

void add_pellets()
{
    if (game_iteration % PELLET_RATE == 0 && num_pellets < MAX_PELLETS) {
        int x = rand() % world_max_x;
        int y = rand() % world_max_y;
        if (get_world_node(x, y)->world_entity == NULL) {
            Pellet* pellet = &pellets[num_pellets++];
            pellet->world_entity.owner_type = PELLET_ENTITY;
            pellet->world_entity.owner = pellet;

            move_to(x, y, &pellet->world_entity);
        }
    }
}

void snake_update()
{
    for (size_t i = 0; i < num_snakes; i++) {
        Snake* snake = &snakes[i];
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

        snake->direction = next_direction;

        int current_x, new_x = current_x = snake->head->x;
        int current_y, new_y = current_y = snake->head->y;

        new_y -= snake->direction == UP ? 1 : 0;
        new_y += snake->direction == DOWN ? 1 : 0;
        new_x -= snake->direction == LEFT ? 1 : 0;
        new_x += snake->direction == RIGHT ? 1 : 0;

        if (new_x > 1 && new_x < world_max_x - 1 && new_y > 1 && new_y < world_max_y - 1) {

            // are we moving into a pellet?
            WorldNode* target_node = get_world_node(new_x, new_y);
            bool eating = target_node->world_entity != NULL
                && target_node->world_entity->owner_type == PELLET_ENTITY;

            if (eating) {
                // flag the pellet as 'gone' by clearing the owner
                target_node->world_entity->owner = NULL;
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
                WorldEntity* node = &snake->nodes[++snake->num_nodes];
                node->owner_type = SNAKE_ENTITY;
                node->owner = snake;
                move_to(current_x, current_y, node);
                snake->num_pellets--;
            }

            if (eating) {
                snake->num_pellets++;
            }
        }
    }
}

bool game_update(int max_x, int max_y)
{
    // TODO: support resize
    assert(world_max_x == max_x & world_max_y == max_y);

    if (game_keys[GAME_KEY_Q] == true) {
        return true;
    }

    add_pellets();
    snake_update();

    game_iteration++;
    return false;
}
