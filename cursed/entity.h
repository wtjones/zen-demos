#ifndef ENTITY_H
#define ENTITY_H

#define MAX_SNAKE_NODES 10

typedef enum Direction {
    UP,
    RIGHT,
    DOWN,
    LEFT
} Direction;

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

typedef struct Wall {
    WorldEntity world_entity;
} Wall;

typedef struct Pellet {
    WorldEntity world_entity;
} Pellet;

typedef struct Snake {
    WorldEntity nodes[MAX_SNAKE_NODES];
    WorldEntity* head;
    int num_nodes;
    int num_pellets;
    int last_move;
    enum Direction direction;
    Direction (*brain)(struct Snake*);
} Snake;

#endif
