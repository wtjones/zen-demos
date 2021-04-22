#include "game.h"
#include "brains/alpha_brain.h"
#include "brains/random_brain.h"
#include "snake.h"
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
    num_snakes = num_snakes <= MAX_SNAKES ? num_snakes : MAX_SNAKES;
    log_info("Spawning in %d snakes", num_snakes);
    for (size_t i = 0; i < num_snakes; i++) {
        Snake* snake = &snakes[i];
        snake->head = &snake->nodes[0];
        snake->head->owner_type = SNAKE_ENTITY;
        snake->head->owner = snake;

        snake->direction = rand() % 4;
        spawn_at(
            (rand() % (world_max_x - 2)) + 1,
            (rand() % (world_max_y - 2)) + 1,
            snake->head);
        snake->num_nodes = 1;
        snake->num_pellets = 0;
        snake->brain = i == 0 ? &run_alpha_brain : &run_random_brain;
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
    spawn_at(x, y, &wall->world_entity);
}

void add_pellets()
{
    if (game_iteration % PELLET_RATE == 0 && num_pellets < MAX_PELLETS) {
        int found_index = -1;
        // find a free entry
        for (size_t i = 0; i < MAX_PELLETS; i++) {
            found_index = entity_exists(&pellets[i].world_entity) ? -1 : i;
            if (found_index >= 0) {
                break;
            }
        }

        int x = rand() % world_max_x;
        int y = rand() % world_max_y;
        bool node_empty = !entity_exists(get_world_node(x, y)->world_entity);
        if (found_index >= 0 && node_empty) {
            Pellet* pellet = &pellets[found_index];
            num_pellets++;
            pellet->world_entity.owner_type = PELLET_ENTITY;
            pellet->world_entity.owner = pellet;
            pellet->world_entity.created_at = game_iteration;

            spawn_at(x, y, &pellet->world_entity);
            log_trace("Pellet added. num_pellets: %d", num_pellets);
        }
    }
}

void pellet_update()
{
    for (size_t i = 0; i < MAX_PELLETS; i++) {
        if (entity_exists(&pellets[i].world_entity)) {
            Pellet* pellet = &pellets[i];

            if (game_iteration - pellet->world_entity.created_at > PELLET_DECAY) {
                clear_entity(&pellet->world_entity);
                num_pellets--;
            }
        }
    }
}

bool game_update(int max_x, int max_y)
{
    // TODO: support resize
    assert(world_max_x == max_x && world_max_y == max_y);

    if (game_keys[GAME_KEY_Q] == true) {
        return true;
    }

    add_pellets();
    pellet_update();
    snakes_update();

    game_iteration++;
    return false;
}
