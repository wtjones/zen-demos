#ifndef TOMBMAP_H
#define TOMBMAP_H

#include "camera.h"
#include "graphics.h"
#include "maths.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#include <larse/core/expression.h>
#include <larse/core/parse.h>
#include <larse/core/repr.h>
#pragma GCC diagnostic pop

#define RAS_TOMBMAP_FLOOR_CEILING_UNIT 256
#define RAS_TOMBMAP_SECTOR_UNITS 1024
#define MAX_TOMBMAP_NAME 50
#define RAS_TOMBMAP_SECTOR_VERTS_MAX 16
#define SCRIPT_SYMBOL_TOMBMAPS "tombmaps"
#define SCRIPT_SYMBOL_TOMBMAP "tombmap"
#define SCRIPT_SYMBOL_TOMBMAP_NAME ":name"
#define SCRIPT_SYMBOL_TOMBMAP_ROOMS "rooms"
#define SCRIPT_SYMBOL_TOMBMAP_ROOM_X ":x"
#define SCRIPT_SYMBOL_TOMBMAP_ROOM_Z ":z"
#define SCRIPT_SYMBOL_TOMBMAP_ROOM_Y_TOP ":y_top"
#define SCRIPT_SYMBOL_TOMBMAP_ROOM_Y_BOTTOM ":y_bottom"
#define SCRIPT_SYMBOL_TOMBMAP_SECTORS "sectors"
#define SCRIPT_SYMBOL_TOMBMAP_SECTOR_MATERIAL ":m"
#define SCRIPT_SYMBOL_TOMBMAP_SECTOR_CEILING ":c"
#define SCRIPT_SYMBOL_TOMBMAP_SECTOR_FLOOR ":f"

typedef enum {
    // UP
    RAS_TOMBMAP_Z_MINUS_1 = 0,
    // DOWN
    RAS_TOMBMAP_Z_PLUS_1 = 1,
    // LEFT
    RAS_TOMBMAP_X_MINUS_1 = 2,
    // RIGHT
    RAS_TOMBMAP_X_PLUS_1 = 3
} RasTombMapSpatial;

/**
 * @brief Sector corners denoted as 00, 01, 11, 10.
 * First digit is X axis, second digit is Z axis.
 *
 * C00           C10
 *        +----+
 *       /      \
 *      /        \
 *     /          \
 *    +------------+
 *  C01            C11
 *
 */
typedef enum {

    FLOOR_BASE_C00,
    FLOOR_BASE_C01,
    FLOOR_BASE_C11,
    FLOOR_BASE_C10,

    FLOOR_TIP_C00,
    FLOOR_TIP_C01,
    FLOOR_TIP_C11,
    FLOOR_TIP_C10,

    CEIL_TIP_C00,
    CEIL_TIP_C01,
    CEIL_TIP_C11,
    CEIL_TIP_C10,

    CEIL_BASE_C00,
    CEIL_BASE_C01,
    CEIL_BASE_C11,
    CEIL_BASE_C10,
} RasTombMapSectorCorner;

typedef struct {
    int32_t material;
    uint8_t spatial_flags;
    int32_t spatial_materials[4];
    int8_t ceiling; // Relative from zero. Each value is 256 world units.
    int8_t floor;   // Relative from zero. Each value is 256 world units.
    /**
     * @brief Vertex indexes for the two possible rectangles of a scector.
     * If ceiling == 0, the tip and base vertices will point to the same index.
     * If floor == 0, the tip and base vertices will point to the same index.
     *
     */
    int32_t corners[RAS_TOMBMAP_SECTOR_VERTS_MAX];
} RasTombMapSector;

typedef struct {
    int32_t x; // world coords
    int32_t z; // world coords
    int32_t y_top;
    int32_t y_bottom;
    RasTombMapSector* sectors;
    size_t num_sectors_x;
    size_t num_sectors_z;
    RasPipelineElement element;
} RasTombMapRoom;

typedef struct {
    char name[MAX_TOMBMAP_NAME];
    RasTombMapRoom* rooms;
    size_t num_rooms;
    uint32_t mesh_index;
} RasSceneTombMap;

RasResult core_script_map_tombmaps(
    LarNode* scene_exp,
    RasSceneTombMap** out_tombmaps,
    size_t* out_num_tombmaps);

/**
 * @brief Create faces by major and minor axis based on camera angle.
 *
 * @param tombmap
 * @param camera
 * @param element
 * @return RasResult
 */
RasResult core_tombmap_to_element_faces(
    RasSceneTombMap* tombmap,
    RasCamera* camera,
    RasPipelineElement* element);

RasResult core_tombmap_to_element_verts(
    RasSceneTombMap* tombmap);

void core_free_scene_tombmaps(RasSceneTombMap* tombmaps, size_t num_tombmaps);

#endif
