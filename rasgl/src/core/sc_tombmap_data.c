#include "rasgl/core/tombmap.h"

// clang-format off
RasTombMapSectorRule g_core_tombmap_sector_pillar_rules[RAS_TOMBMAP_SECTOR_SPACE_COUNT][RAS_TOMBMAP_SPATIAL_COUNT] =
{
 [RAS_TOMBMAP_SECTOR_SPACE_FLOOR] = {

    // Create interior far face in CCW order.
    // Looking far:
    // N01 -- N11
    // |       |
    // |       |
    // C00 -- C10
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
    },

    // Create interior left face in CCW order.
    // Looking left:
    // N11 -- N10
    // |          |
    // |          |
    // C01 -- C00
    [RAS_TOMBMAP_X_MINUS_1] = {
        .verts = {
            { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, FLOOR_TIP_C10 },
            { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, FLOOR_TIP_C11 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C01 },
            { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, FLOOR_TIP_C10 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C01 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C00 }
        },
        .num_verts = 6
    },

    // Create interior near face in CCW order.
    // Looking near:
    // N10 -- CT00
    // |       |
    // |       |
    // C11 -- C01
    [RAS_TOMBMAP_Z_PLUS_1] = {
        .verts = {
            { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, FLOOR_TIP_C00 },
            { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, FLOOR_TIP_C10 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C11 },
            { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, FLOOR_TIP_C00 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C11 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C01 }
        },
        .num_verts = 6
    },
    // Create interior right face in CCW order.
    // Looking right:
    // N00 -- N01
    // |       |
    // |       |
    // C10 -- C11
    [RAS_TOMBMAP_X_PLUS_1] = {
        .verts = {
            { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, FLOOR_TIP_C01 },
            { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, FLOOR_TIP_C00 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C10 },
            { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, FLOOR_TIP_C01 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C10 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C11 }
        },
        .num_verts = 6
    },
},
[RAS_TOMBMAP_SECTOR_SPACE_CEILING] = {
    // Create interior far face in CCW order.
    // Looking far:
    // C00 -- C10
    // |       |
    // |       |
    // N01 -- N11
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
    },

      // Create interior left face in CCW order.
            // Looking left:
            // C01 -- C00
            // |         |
            // |         |
            // N11 -- N10
    [RAS_TOMBMAP_X_MINUS_1] = {
        .verts = {
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C00 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C01 },
            { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, CEIL_TIP_C11 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C00 },
            { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, CEIL_TIP_C11 },
            { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, CEIL_TIP_C10 }
           },
        .num_verts = 6
    },
    // Create interior near face in CCW order.
    // Looking near:
    // C11 -- CT01
    // |       |
    // |       |
    // N10 -- N00
    [RAS_TOMBMAP_Z_PLUS_1] = {
        .verts = {
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C01 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C11 },
            { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, CEIL_TIP_C10 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C01 },
            { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, CEIL_TIP_C10 },
            { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, CEIL_TIP_C00 }
        },
        .num_verts = 6
    },
    // Create interior right face in CCW order.
    // Looking right:
    // C10 -- C11
    // |       |
    // |       |
    // N00 -- N01
    [RAS_TOMBMAP_X_PLUS_1] = {
        .verts = {
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C11 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C10 },
            { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, CEIL_TIP_C00 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C11 },
            { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, CEIL_TIP_C00 },
            { RAS_TOMBMAP_SECTOR_TARGET_NEIGHBOR, CEIL_TIP_C01 }
        },
        .num_verts = 6
    }
}};

RasTombMapSectorRule g_core_tombmap_sector_wall_rules[RAS_TOMBMAP_SPATIAL_COUNT] = {
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
    },

    // Create interior left wall in CCW order.
    // Looking left:
    // CT01 -- CT00
    // |          |
    // |          |
    // FT01 -- FT00
    [RAS_TOMBMAP_X_MINUS_1] = {
        .verts = {
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C00 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C01 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C01 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C00 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C01 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C00 }
            },
        .num_verts = 6
    },

    // Create interior near wall in CCW order.
    // Looking near:
    // CT11 -- CT01
    // |       |
    // |       |
    // FT11 -- FT01
    [RAS_TOMBMAP_Z_PLUS_1] = {
        .verts = {
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C01 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C11 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C11 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C01 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C11 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C01 }
        },
        .num_verts = 6
    },
    // Create interior right wall in CCW order.
    // Looking right:
    // CT10 -- CT11
    // |       |
    // |       |
    // FT10 -- FT11
    [RAS_TOMBMAP_X_PLUS_1] = {
        .verts = {
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C11 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C10 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C10 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, CEIL_TIP_C11 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C10 },
            { RAS_TOMBMAP_SECTOR_TARGET_CURRENT, FLOOR_TIP_C11 }
        },
        .num_verts = 6
    }
};

// clang-format on
