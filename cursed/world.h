#ifndef WORLD_H
#define WORLD_H

#define WORLD_MAX_X 300
#define WORLD_MAX_Y 200

#include "entity.h"
#include <stdbool.h>

extern int world_max_x,
    world_max_y;

void world_init(int max_x, int max_y);
void world_cleanup();
bool in_world_bounds(int x, int y);
WorldNode* get_world_node(int x, int y);
void spawn_at(int x, int y, WorldEntity* entity);
void move_to(int x, int y, WorldEntity* entity);
bool entity_exists(WorldEntity* entity);
bool entity_type_exists(WorldEntity* entity, GameEntityType type);
WorldEntity* get_entity_type(int x, int y);
WorldEntity* get_entity_at(int x, int y);
void clear_entity(WorldEntity* entity);

#endif
