#ifndef WORLD_H
#define WORLD_H

#define WORLD_MAX_X 300
#define WORLD_MAX_Y 200

#include <stdbool.h>

typedef enum GameEntityType {
    NO_ENTITY,
    WALL_ENTITY,
    SNAKE_ENTITY,
    FOOD_ENTITY
} GameEntityType;

typedef struct WorldEntity {
    GameEntityType owner_type;
    void* owner;
    int x;
    int y;
} WorldEntity;

typedef struct WorldNode {
    WorldEntity* world_entity;
} WorldNode;

extern int world_max_x,
    world_max_y;

void world_init(int max_x, int max_y);
WorldNode* get_world_node(int x, int y);
void move_to(int x, int y, WorldEntity* entity);
#endif
