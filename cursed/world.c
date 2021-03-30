
#include "world.h"
#include <assert.h>
#include <stdlib.h>

int world_max_x,
    world_max_y;
WorldNode world_nodes[WORLD_MAX_X][WORLD_MAX_Y];

void world_init(int max_x, int max_y)
{
    world_max_x = max_x <= WORLD_MAX_X ? max_x : WORLD_MAX_X;
    world_max_y = max_y <= WORLD_MAX_Y ? max_y : WORLD_MAX_Y;

    //world_nodes = malloc(sizeof(WorldNode*) * max_x * max_y);
    for (size_t y = 0; y < world_max_y; y++) {
        for (size_t x = 0; x < world_max_x; x++) {
            WorldNode* node = &world_nodes[x][y];
            node->world_entity = NULL;
        }
    }
}

WorldNode* get_world_node(int x, int y)
{
    //return &world_nodes[y * world_max_x + x];
    return &world_nodes[x][y];
}
void move_to(int x, int y, WorldEntity* entity)
{
    world_nodes[entity->x][entity->y].world_entity = NULL;
    entity->x = x;
    entity->y = y;
    world_nodes[x][y].world_entity = entity;
    //world_nodes[y * world_max_x + x].world_entity = NULL; //entity;
}
