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
// FIXME: Should be sub units of a sector.
#define RAS_TOMBMAP_SECTOR_PILLAR_UNITS RAS_FIXED_QUARTER
#define MAX_TOMBMAP_NAME 50
#define RAS_TOMBMAP_SECTOR_VERTS_MAX 16
#define RAS_TOMBMAP_WALL_VALUE (-127)
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
    RAS_TOMBMAP_X_PLUS_1 = 3,
    RAS_TOMBMAP_SPATIAL_COUNT = 4
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

typedef enum {
    RAS_TOMBMAP_BASE,
    RAS_TOMBMAP_TIP
} RasTombmapPillarSpace;

typedef enum {
    RAS_TOMBMAP_SECTOR_SPACE_FLOOR,
    RAS_TOMBMAP_SECTOR_SPACE_CEILING,
    RAS_TOMBMAP_SECTOR_SPACE_COUNT
} RasTombMapSectorSpace;

typedef enum {
    RAS_TOMBMAP_C00,
    RAS_TOMBMAP_C01,
    RAS_TOMBMAP_C11,
    RAS_TOMBMAP_C10
} RasTombMapSectorCorner2;

typedef enum {
    RAS_TOMBMAP_RULE_FLOOR,
    RAS_TOMBMAP_RULE_FLOOR_PILLAR,
    RAS_TOMBMAP_RULE_CEILING,
    RAS_TOMBMAP_RULE_WALL,
    RAS_TOMBMAP_RULE_COUNT
} RasTombMapSectorFaceRuleType;

typedef enum {
    RAS_TOMBMAP_SECTOR_TARGET_CURRENT,
    RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR
} RasTombMapSectorTarget;
/*
PILLAR_Z_MINUS_1, [
        RAS_TOMBMAP_SECTOR_TARGET_CURRENT, RAS_TOMBMAP_C00,
        RAS_TOMBMAP_SECTOR_TARGET_CURRENT, RAS_TOMBMAP_C01,
       RAS_TOMBMAP_SECTOR_TARGET_CURRENT, RAS_TOMBMAP_C11]
*/

typedef struct {
    RasTombMapSectorTarget target;
    RasTombMapSectorCorner corner;
} RasTombMapSectorRuleVert;

typedef struct {
    RasTombMapSectorRuleVert verts[6];
    size_t num_verts;

} RasTombMapSectorRule;

typedef struct {
    RasTombMapSectorRule* rules[4];
    size_t num_rules;
} RasTombMapSectorRuleResult;

// add_sector_face0(
//                 element,
//                 sector->corners[FLOOR_TIP_C10],
//                 sector->corners[FLOOR_TIP_C00],
//                 sector->corners[FLOOR_TIP_C01],
//                 checker);
//             add_sector_face1(
//                 element,
//                 sector->corners[FLOOR_TIP_C10],
//                 sector->corners[FLOOR_TIP_C01],
//                 sector->corners[FLOOR_TIP_C11],
//                 checker);

// clang-format off

#define RAS_TOMBMAP_RULE_FLOOR_VERTS \
    { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C10 }, \
    { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C00 }, \
    { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C01 }, \
    { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C10 }, \
    { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C01 }, \
    { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C11 } 

// static  RasTombMapSectorRule core_tombmap_sector_side_rules[RAS_TOMBMAP_RULE_COUNT][RAS_TOMBMAP_SPATIAL_COUNT] = {
//     // RAS_TOMBMAP_RULE_WALL
//     {
//         // Z_MINUS_1
//         {
//             .verts = {
//                 { .target = RAS_TOMBMAP_SECTOR_TARGET_CURRENT, .corner = CEIL_TIP_C10 },
//                 { .target = RAS_TOMBMAP_SECTOR_TARGET_CURRENT, .corner = CEIL_TIP_C00 },
//                 { .target = RAS_TOMBMAP_SECTOR_TARGET_CURRENT, .corner = FLOOR_TIP_C00 },
//                 { .target = RAS_TOMBMAP_SECTOR_TARGET_CURRENT, .corner = CEIL_TIP_C10 },
//                 { .target = RAS_TOMBMAP_SECTOR_TARGET_CURRENT, .corner = FLOOR_TIP_C00 },
//                 { .target = RAS_TOMBMAP_SECTOR_TARGET_CURRENT, .corner = FLOOR_TIP_C10 } },
//             .num_verts = 6,
//         } },
//     // RAS_TOMBMAP_RULE_FLOOR
//     { 
//         // Z_MINUS_1
//         {
//             .verts = {
             
//                 { .target = RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, .corner = FLOOR_TIP_C11 },
//                 { .target = RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, .corner = FLOOR_TIP_C01 },
//                 { .target = RAS_TOMBMAP_SECTOR_TARGET_CURRENT, .corner = FLOOR_TIP_C00 },
//                 { .target = RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, .corner = FLOOR_TIP_C11 },
//                 { .target = RAS_TOMBMAP_SECTOR_TARGET_CURRENT, .corner = FLOOR_TIP_C00 },
//                 { .target = RAS_TOMBMAP_SECTOR_TARGET_CURRENT, .corner = FLOOR_TIP_C10 }
//             },
//             .num_verts = 12,
//         } }
// };
// clang-format on

// clang-format off

extern RasTombMapSectorRule g_core_tombmap_sector_tile_rules[RAS_TOMBMAP_SECTOR_SPACE_COUNT];
extern RasTombMapSectorRule g_core_tombmap_sector_pillar_rules[RAS_TOMBMAP_SECTOR_SPACE_COUNT][RAS_TOMBMAP_SPATIAL_COUNT];
extern RasTombMapSectorRule g_core_tombmap_sector_wall_rules[RAS_TOMBMAP_SPATIAL_COUNT];


static const RasTombMapSectorRule core_tombmap_sector_side_rules[RAS_TOMBMAP_RULE_COUNT][RAS_TOMBMAP_SPATIAL_COUNT] = {
    
    [RAS_TOMBMAP_RULE_FLOOR] = {
        [RAS_TOMBMAP_Z_MINUS_1] = {
            .verts = {
                { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, FLOOR_TIP_C11 },
                { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, FLOOR_TIP_C01 },
                { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C00 },
                { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, FLOOR_TIP_C11 },
                { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C00 },
                { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C10 }
            },
            .num_verts = 6
        }
    },
    [RAS_TOMBMAP_RULE_CEILING] = {
        [RAS_TOMBMAP_Z_MINUS_1] = {
            .verts = {
                { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C10 },
                { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C00 },
                { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, CEIL_TIP_C01 },
                { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C10 },
                { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, CEIL_TIP_C01 },
                { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, CEIL_TIP_C11 }
            },
            .num_verts = 6
        }
    },
    [RAS_TOMBMAP_RULE_WALL] = {
        [RAS_TOMBMAP_Z_MINUS_1] = {
            .verts = {
                { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C10 },
                { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C00 },
                { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C00 },
                { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C10 },
                { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C00 },
                { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C10 }
            },
            .num_verts = 6
        }
    }
};
// clang-format on

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
    uint32_t mesh_index;
} RasTombMapRoom;

static inline bool core_tombmap_sector_is_floor_pillar(RasTombMapSector* sector)
{
    return sector == NULL
        ? false
        : sector->floor != RAS_TOMBMAP_WALL_VALUE && sector->floor != 0;
}

static inline bool core_tombmap_sector_is_ceiling_pillar(RasTombMapSector* sector)
{
    return sector == NULL
        ? false
        : sector->ceiling != RAS_TOMBMAP_WALL_VALUE && sector->ceiling != 0;
}

static inline bool core_tombmap_sector_is_wall(RasTombMapSector* sector)
{
    return sector == NULL
        ? false
        : sector->floor == RAS_TOMBMAP_WALL_VALUE && sector->ceiling == RAS_TOMBMAP_WALL_VALUE;
}

/**
 * @brief Determine if the side between sector and neighbor is visible.
 * The neighbor is visible if it is a wall or has a higher floor.
 *
 * @param sector
 * @param neighbor
 * @return true
 * @return false
 */
static inline bool core_tombmap_floor_side_visible(
    RasTombMapSector* sector,
    RasTombMapSector* neighbor)
{
    if (sector == NULL && neighbor == NULL) {
        return false;
    }

    return core_tombmap_sector_is_wall(neighbor)
        || (neighbor->floor > sector->floor);
}

/**
 * @brief Determine if the side between sector and neighbor is visible.
 * The neighbor is visible if it is a wall or has a higher floor.
 *
 * @param sector
 * @param neighbor
 * @return true
 * @return false
 */
static inline bool core_tombmap_ceiling_side_visible(
    RasTombMapSector* sector,
    RasTombMapSector* neighbor)
{
    if (sector == NULL && neighbor == NULL) {
        return false;
    }

    return !core_tombmap_sector_is_wall(neighbor)
        && (neighbor->ceiling < sector->ceiling);
}

static inline RasTombMapSector* core_get_sector(RasTombMapRoom* room, int32_t x, int32_t z)
{
    if (x < 0 || x >= (int32_t)room->num_sectors_x || z < 0 || z >= (int32_t)room->num_sectors_z) {
        return NULL;
    }
    return &room->sectors[(z * room->num_sectors_x) + x];
}

typedef struct {
    char name[MAX_TOMBMAP_NAME];
    RasTombMapRoom* rooms;
    size_t num_rooms;
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
RasResult core_tombmap_room_to_element_faces(
    RasTombMapRoom* room,
    RasCamera* camera,
    RasPipelineElement* element);

RasResult core_tombmap_to_element_verts(
    RasSceneTombMap* tombmap);

void core_free_scene_tombmaps(RasSceneTombMap* tombmaps, size_t num_tombmaps);

#endif
