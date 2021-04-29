#include "pellet.h"
#include "game.h"
#include <stdlib.h>

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
