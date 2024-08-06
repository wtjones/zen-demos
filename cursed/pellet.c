#include "pellet.h"
#include "game.h"
#include <stdlib.h>

void add_pellet(int x, int y)
{

    int found_index = -1;
    for (size_t i = 0; i < MAX_PELLETS; i++) {
        found_index = entity_exists(&pellets[i].world_entity) ? -1 : i;
        if (found_index >= 0) {
            break;
        }
    }

    bool node_empty = !entity_exists(get_world_node(x, y)->world_entity);

    if (found_index >= 0 && node_empty) {
        Pellet* pellet = &pellets[found_index];
        num_pellets++;
        pellet->world_entity.owner_type = PELLET_ENTITY;
        pellet->world_entity.owner = pellet;
        pellet->world_entity.created_at = game_iteration;

        spawn_at(x, y, &pellet->world_entity);
        log_info("Pellet added. x = %d, y = %d, num_pellets: %d", x, y, num_pellets);
    }
}

void add_pellets()
{

    if (num_pellets == MAX_PELLETS) {
        return;
    }

    int x, y;
    if (game_iteration % PELLET_RATE == 0 && num_pellets < MAX_PELLETS) {
        x = rand() % world_max_x;
        y = rand() % world_max_y;
        add_pellet(x, y);
    }

    if (game_keys[GAME_KEY_MOUSE1] == 1) {
        x = mouse_x;
        y = mouse_y;
        log_info("Adding pellet at x: %d, y: %d", x, y);
        add_pellet(x, y);
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
