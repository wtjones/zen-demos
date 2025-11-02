#ifndef GRIDMAP_H
#define GRIDMAP_H

#include "graphics.h"
#include "maths.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#include <larse/core/expression.h>
#include <larse/core/parse.h>
#include <larse/core/repr.h>
#pragma GCC diagnostic pop

#define RAS_MAX_GRIDMAP_WIDTH 128
#define RAS_MAX_GRIDMAP_HEIGHT 1
#define RAS_MAX_GRIDMAP_DEPTH 128
#define MAX_GRIDMAP_NAME 50
#define MAX_FILE_PATH 50
#define SCRIPT_SYMBOL_GRIDMAPS "gridmaps"
#define SCRIPT_SYMBOL_GRIDMAP "gridmap"
#define SCRIPT_SYMBOL_GRIDMAP_NAME ":name"
#define SCRIPT_SYMBOL_GRIDMAP_WIDTH ":width"
#define SCRIPT_SYMBOL_GRIDMAP_HEIGHT ":height"
#define SCRIPT_SYMBOL_GRIDMAP_DEPTH ":depth"
#define SCRIPT_SYMBOL_GRIDMAP_CELL_SIZE ":cell_size"
#define SCRIPT_SYMBOL_GRIDMAP_CELLS ":cells"

typedef enum {
    // UP
    RAS_GRIDMAP_Z_MINUS_1 = 0,
    // DOWN
    RAS_GRIDMAP_Z_PLUS_1 = 1,
    // LEFT
    RAS_GRIDMAP_X_MINUS_1 = 2,
    // RIGHT
    RAS_GRIDMAP_X_PLUS_1 = 3
} RasGridMapSpatial;

typedef struct {
    int32_t material;
    uint8_t spatial_flags;
    int32_t spatial_materials[4];
} RasGridMapCell;

typedef struct {
    char name[MAX_GRIDMAP_NAME];
    size_t width;
    size_t height;
    size_t depth;
    RasFixed cell_size;
    RasGridMapCell cells[RAS_MAX_GRIDMAP_WIDTH * RAS_MAX_GRIDMAP_HEIGHT * RAS_MAX_GRIDMAP_DEPTH];
    RasPipelineElement element;
    uint32_t mesh_index;
} RasSceneGridMap;

/**
 * @brief Map a gridmap script to a RasGridMap
 *
 * @param script The parsed larse script
 * @return RasResult
 */
RasResult core_script_map_gridmap(LarNode* gridmap_exp, RasSceneGridMap* gridmap);

#endif
