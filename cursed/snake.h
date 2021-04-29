#ifndef SNAKE_H
#define SNAKE_H

#include "entity.h"
#include "util.h"
#include <stdbool.h>

#define INITIAL_SPEED 10
#define SPEED_PER_NODE 2
#define MINIMUM_SPEED 5

int get_speed(Snake* snake);
void snake_update(Snake* snake);
void snakes_update();

#endif
