#ifndef GRIDMAP_H
#define GRIDMAP_H

#include "maths.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#include <larse/core/expression.h>
#include <larse/core/parse.h>
#include <larse/core/repr.h>
#pragma GCC diagnostic pop

#define RAS_MAX_GRIDMAP_WIDTH 128
#define RAS_MAX_GRIDMAP_HEIGHT 128
#define MAX_GRIDMAP_NAME 50
#define MAX_FILE_PATH 50
#define SCRIPT_SYMBOL_GRIDMAPS "gridmaps"
#define SCRIPT_SYMBOL_GRIDMAP "gridmap"
#define SCRIPT_SYMBOL_GRIDMAP_NAME ":name"
#define SCRIPT_SYMBOL_GRIDMAP_WIDTH ":width"
#define SCRIPT_SYMBOL_GRIDMAP_HEIGHT ":height"
#define SCRIPT_SYMBOL_GRIDMAP_CELL_SIZE ":cell_size"
#define SCRIPT_SYMBOL_GRIDMAP_CELLS ":cells"

typedef enum {
    RAS_GRIDMAP_UP = 1 << 0,
    RAS_GRIDMAP_DOWN = 1 << 1,
    RAS_GRIDMAP_LEFT = 1 << 2,
    RAS_GRIDMAP_RIGHT = 1 << 3
} RasGridMapSpacialFlags;

typedef struct {
    char name[MAX_GRIDMAP_NAME];
    size_t width;
    size_t height;
    RasFixed cell_size;
    int cells[RAS_MAX_GRIDMAP_WIDTH * RAS_MAX_GRIDMAP_HEIGHT];
    uint8_t spacial_flags;
} RasSceneGridMap;

/**
 * @brief Map a gridmap script to a RasGridMap
 *
 * @param script The parsed larse script
 * @return RasResult
 */
RasResult core_script_map_gridmap(LarNode* gridmap_exp, RasSceneGridMap* gridmap);

#endif
