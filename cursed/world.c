
#include "world.h"
#include <assert.h>
#include <stdlib.h>

int world_max_x,
    world_max_y;
WorldNode* world_nodes;

void world_init(int max_x, int max_y)
{
    world_max_x = max_x <= WORLD_MAX_X ? max_x : WORLD_MAX_X;
    world_max_y = max_y <= WORLD_MAX_Y ? max_y : WORLD_MAX_Y;

    world_nodes = malloc(sizeof(WorldNode*) * max_x * max_y);
    for (size_t y = 0; y < world_max_y; y++) {
        for (size_t x = 0; x < world_max_x; x++) {
            WorldNode* node = get_world_node(x, y);
            node->world_entity = NULL;
        }
    }
}

void world_cleanup()
{
    free(world_nodes);
}

WorldNode* get_world_node(int x, int y)
{
    return &world_nodes[y * world_max_x + x];
}

/**
 * Set a new entity in the world
 *
 */
void spawn_at(int x, int y, WorldEntity* entity)
{
    WorldNode* node = get_world_node(x, y);
    entity->x = x;
    entity->y = y;
    node->world_entity = entity;
}

/**
 * Move an existing entity
 *
 */
void move_to(int x, int y, WorldEntity* entity)
{
    assert(entity->x >= 0 && entity->y >= 0);
    WorldNode* node = get_world_node(entity->x, entity->y);
    node->world_entity = NULL;
    spawn_at(x, y, entity);
}

bool entity_exists(WorldEntity* entity)
{
    return entity != NULL && entity->owner_type != NO_ENTITY;
}

bool entity_type_exists(WorldEntity* entity, GameEntityType type)
{
    return entity_exists(entity) && entity->owner_type == type;
}

void clear_entity(WorldEntity* entity)
{
    entity->created_at = 0;
    entity->owner = NULL;
    entity->owner_type = NO_ENTITY;
    entity->x = -1;
    entity->y = -1;
}