#ifndef WORLD_H
#define WORLD_H

#define WORLD_MAX_X 300
#define WORLD_MAX_Y 200

#include <stdbool.h>

typedef enum GameEntityType {
    NO_ENTITY,
    WALL_ENTITY,
    SNAKE_ENTITY,
    PELLET_ENTITY
} GameEntityType;

typedef struct WorldEntity {
    GameEntityType owner_type;
    void* owner;
    int x;
    int y;
    int created_at;
} WorldEntity;

typedef struct WorldNode {
    WorldEntity* world_entity;
} WorldNode;

extern int world_max_x,
    world_max_y;

void world_init(int max_x, int max_y);
void world_cleanup();
WorldNode* get_world_node(int x, int y);
void move_to(int x, int y, WorldEntity* entity);
bool entity_exists(WorldEntity* entity);
void clear_entity(WorldEntity* entity);
#endif
