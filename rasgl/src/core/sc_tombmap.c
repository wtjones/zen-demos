
#include "rasgl/core/graphics.h"
#include "rasgl/core/tombmap.h"

extern RasTombMapSectorRule g_core_tombmap_sector_pillar_rules[RAS_TOMBMAP_SECTOR_SPACE_COUNT][RAS_TOMBMAP_SPATIAL_COUNT];
extern RasTombMapSectorRule g_core_tombmap_sector_wall_rules[RAS_TOMBMAP_SPATIAL_COUNT];

static inline void add_sector_face(
    RasPipelineElement* element,
    uint32_t v0i,
    uint32_t v1i,
    uint32_t v2i,
    uint32_t material_index)
{
    element->indexes[element->num_indexes++] = v0i;
    element->indexes[element->num_indexes++] = v1i;
    element->indexes[element->num_indexes++] = v2i;
    element->material_indexes[element->num_material_indexes++] = material_index;

    element->faces[element->num_faces].material_index = material_index;
    core_face_normal(
        &element->verts[v0i].position,
        &element->verts[v1i].position,
        &element->verts[v2i].position,
        &element->faces[element->num_faces].normal);
    element->faces[element->num_faces].outline_edges = 0;
    element->faces[element->num_faces].outline_material_index = 0;
    element->num_faces++;
}

static inline void add_sector_face0(
    RasPipelineElement* element,
    uint32_t v0i,
    uint32_t v1i,
    uint32_t v2i,
    uint32_t material_index)
{
    uint32_t face_index = element->num_faces;

    RasVector3f* v0 = &element->verts[v0i].position;
    RasVector3f* v1 = &element->verts[v1i].position;
    RasVector3f* v2 = &element->verts[v2i].position;

    add_sector_face(
        element,
        v0i,
        v1i,
        v2i,
        material_index);
    // Outline edges 0 and 1.
    element->faces[face_index].outline_edges = 1 | 2;
    element->faces[face_index].outline_material_index = 0;
}

static inline void add_sector_face1(
    RasPipelineElement* element,
    uint32_t v0i,
    uint32_t v1i,
    uint32_t v2i,
    uint32_t material_index)
{
    uint32_t face_index = element->num_faces;

    add_sector_face(
        element,
        v0i,
        v1i,
        v2i,
        material_index);
    // Outline edges 1 and 2.
    element->faces[face_index].outline_edges = 2 | 4;
    element->faces[face_index].outline_material_index = 0;
}

static inline void add_sector_faces(
    RasPipelineElement* element,
    RasTombMapSector* sector,
    RasTombMapSector* neighbor,
    RasTombMapSectorRuleResult* rules)
{
    if (rules == NULL) {
        return;
    }

    for (size_t r = 0; r < rules->num_rules; r++) {
        RasTombMapSectorRule* rule = rules->rules[r];
        for (size_t i = 0; i < rule->num_verts; i += 6) {
            RasTombMapSector* v0_sector = rule->verts[i].target == RAS_TOMBMAP_SECTOR_TARGET_CURRENT
                ? sector
                : neighbor;
            RasTombMapSector* v1_sector = rule->verts[i + 1].target == RAS_TOMBMAP_SECTOR_TARGET_CURRENT
                ? sector
                : neighbor;
            RasTombMapSector* v2_sector = rule->verts[i + 2].target == RAS_TOMBMAP_SECTOR_TARGET_CURRENT
                ? sector
                : neighbor;
            add_sector_face0(
                element,
                v0_sector->corners[rule->verts[i].corner],
                v1_sector->corners[rule->verts[i + 1].corner],
                v2_sector->corners[rule->verts[i + 2].corner],
                neighbor->material);

            RasTombMapSector* v3_sector = rule->verts[i + 3].target == RAS_TOMBMAP_SECTOR_TARGET_CURRENT
                ? sector
                : neighbor;
            RasTombMapSector* v4_sector = rule->verts[i + 4].target == RAS_TOMBMAP_SECTOR_TARGET_CURRENT
                ? sector
                : neighbor;
            RasTombMapSector* v5_sector = rule->verts[i + 5].target == RAS_TOMBMAP_SECTOR_TARGET_CURRENT
                ? sector
                : neighbor;
            add_sector_face1(
                element,
                v3_sector->corners[rule->verts[i + 3].corner],
                v4_sector->corners[rule->verts[i + 4].corner],
                v5_sector->corners[rule->verts[i + 5].corner],
                neighbor->material);
        }
    }
}

/**
 * @brief Get face construction rule(s) for a side.
 *
 *
 * get floor tile - always - not here
 * get ceiling tile - always - not here
 * get floor pillar - if not same height as neighbor
 * get ceiling pillar - if not same height as neighbor
 * get wall - if neighbor is wall
 *
 * @param direction
 * @param sector
 * @param neighbor
 */
void core_tombmap_get_side_rules(
    RasTombMapSpatial direction,
    RasTombMapSector* sector,
    RasTombMapSector* neighbor,
    RasTombMapSectorRuleResult* result)
{
    result->num_rules = 0;

    if (core_tombmap_sector_is_wall(sector)) {
        return;
    }

    if (core_tombmap_sector_is_wall(neighbor)) {
        result->rules[result->num_rules++] = &g_core_tombmap_sector_wall_rules[direction];
        return;
    }

    if (core_tombmap_floor_side_visible(sector, neighbor)) {
        result->rules[result->num_rules++] = &g_core_tombmap_sector_pillar_rules[RAS_TOMBMAP_SECTOR_SPACE_FLOOR][direction];
    }
    if (core_tombmap_ceiling_side_visible(sector, neighbor)) {
        result->rules[result->num_rules++] = &g_core_tombmap_sector_pillar_rules[RAS_TOMBMAP_SECTOR_SPACE_CEILING][direction];
    }
}

RasResult core_tombmap_room_to_element_faces(
    RasTombMapRoom* room,
    RasCamera* camera,
    RasPipelineElement* element)
{
    element->num_indexes = 0;
    element->num_faces = 0;
    element->num_material_indexes = 0;

    // Track the far-back-left and far-top-left vert indexes
    // size_t fbl_index = 0;

    size_t z_cell = 0, x_cell = 0;
    size_t major_i = 0;
    size_t minor_i = 0;
    int32_t major_axis; // 0 = x, 1 = z
    int32_t major_dir;  // 0 = 0..len, 1 = len..0
    int32_t minor_dir;  // 0 = 0..len, 1 = len..0

    /*
        Camera angles from top-down:

                -Z
           225  180  135
                 |
        -X 270 --+-- 90 +X
                 |
           315   0   45
                +Z
    */

    RasOctant octant = core_angle_to_octant(camera->angle);

    switch (octant) {
    case DIR_N:
        // Looking down -Z, toward -X
        major_axis = 1;
        major_dir = 0;
        minor_dir = 0;
        ras_log_buffer("Looking down -Z");
        break;
    case DIR_NW:
        // Looking down -X, toward -Z
        major_axis = 0;
        major_dir = 0;
        minor_dir = 0;
        ras_log_buffer("Looking down -X");
        break;
    case DIR_W:
        // Looking down -X, toward +Z
        major_axis = 0;
        major_dir = 0;
        minor_dir = 1;
        ras_log_buffer("Looking down -X");
        break;
    case DIR_SW:
        // Looking down +Z, toward -X
        major_axis = 1;
        major_dir = 1;
        minor_dir = 0;
        ras_log_buffer("Looking down -X");
        break;
    case DIR_S:
        // Looking down +Z, toward +X
        major_axis = 1;
        major_dir = 1;
        minor_dir = 1;
        ras_log_buffer("Looking down +Z");
        break;
    case DIR_SE:
        // Looking down +X, toward +Z
        major_axis = 0;
        major_dir = 1;
        minor_dir = 1;
        ras_log_buffer("Looking down +X");
        break;
    case DIR_E:
        // Looking down +X, toward -Z
        major_axis = 0;
        major_dir = 1;
        minor_dir = 0;
        ras_log_buffer("Looking down +X");
        break;
    case DIR_NE:
        // Looking down -Z, toward +X
        major_axis = 1;
        major_dir = 0;
        minor_dir = 1;
        ras_log_buffer("Looking down -Z");
        break;
    default:
        ras_log_error("Invalid octant %d", octant);
        return RAS_RESULT_ERROR;
    }

    const size_t major_length = major_axis == 0 ? room->num_sectors_x : room->num_sectors_z;
    const size_t minor_length = major_axis == 0 ? room->num_sectors_z : room->num_sectors_x;
    for (major_i = 0; major_i < major_length; major_i++) {
        for (minor_i = 0; minor_i < minor_length; minor_i++) {
            if (major_axis == 0) {
                x_cell = major_dir == 0 ? major_i : (major_length - 1 - major_i);
                z_cell = minor_dir == 0 ? minor_i : (minor_length - 1 - minor_i);
            } else {
                z_cell = major_dir == 0 ? major_i : (major_length - 1 - major_i);
                x_cell = minor_dir == 0 ? minor_i : (minor_length - 1 - minor_i);
            }

            RasTombMapSector* sector = core_get_sector(room, x_cell, z_cell);
            RasTombMapSector* sector_x_minus = core_get_sector(room, x_cell - 1, z_cell);
            RasTombMapSector* sector_x_plus = core_get_sector(room, x_cell + 1, z_cell);
            RasTombMapSector* sector_z_minus = core_get_sector(room, x_cell, z_cell - 1);
            RasTombMapSector* sector_z_plus = core_get_sector(room, x_cell, z_cell + 1);

            if (core_tombmap_sector_is_wall(sector)) {
                continue; // wall
            }

            // Create interior floor face in CCW order.
            // Looking down:
            // C00 -- C10
            // |       |
            // |       |
            // C01 -- C11
            // Order: (C10, C00, C01), (C10, C01, C11)

            uint32_t checker = (z_cell + x_cell) % 2 == 0
                ? 0
                : 3;
            add_sector_face0(
                element,
                sector->corners[FLOOR_TIP_C10],
                sector->corners[FLOOR_TIP_C00],
                sector->corners[FLOOR_TIP_C01],
                checker);
            add_sector_face1(
                element,
                sector->corners[FLOOR_TIP_C10],
                sector->corners[FLOOR_TIP_C01],
                sector->corners[FLOOR_TIP_C11],
                checker);

            // Create interior ceiling face in CCW order.
            // Looking up:
            // C01 -- C11
            // |       |
            // |       |
            // C00 -- C10
            // Order: (C11, C01, C00), (C11, C00, C10)

            add_sector_face0(
                element,
                sector->corners[CEIL_TIP_C11],
                sector->corners[CEIL_TIP_C01],
                sector->corners[CEIL_TIP_C00],
                checker);
            add_sector_face1(
                element,
                sector->corners[CEIL_TIP_C11],
                sector->corners[CEIL_TIP_C00],
                sector->corners[CEIL_TIP_C10],
                checker);

            RasTombMapSectorRuleResult rules;

            core_tombmap_get_side_rules(
                RAS_TOMBMAP_Z_MINUS_1,
                sector,
                sector_z_minus,
                &rules);

            add_sector_faces(
                element,
                sector,
                sector_z_minus,
                &rules);

            core_tombmap_get_side_rules(
                RAS_TOMBMAP_X_MINUS_1,
                sector,
                sector_x_minus,
                &rules);

            add_sector_faces(
                element,
                sector,
                sector_x_minus,
                &rules);

            core_tombmap_get_side_rules(
                RAS_TOMBMAP_Z_PLUS_1,
                sector,
                sector_z_plus,
                &rules);

            add_sector_faces(
                element,
                sector,
                sector_z_plus,
                &rules);

            core_tombmap_get_side_rules(
                RAS_TOMBMAP_X_PLUS_1,
                sector,
                sector_x_plus,
                &rules);

            add_sector_faces(
                element,
                sector,
                sector_x_plus,
                &rules);
        }
    }
    return RAS_RESULT_OK;
}

void core_add_floor_pillar_corner(
    RasTombMapRoom* room,
    RasTombMapSector* sector,
    size_t corner_index,
    int32_t x,
    int32_t z)
{
    if (sector != NULL && core_tombmap_sector_is_floor_pillar(sector)) {

        // Pillar tip vertex
        int32_t vert_index = room->element.num_verts++;
        RasVector3f* v = &room->element.verts[vert_index].position;

        v->x = INT_32_TO_FIXED_16_16(room->x + (x * 1));
        v->y = INT_32_TO_FIXED_16_16(sector->floor);
        v->z = INT_32_TO_FIXED_16_16(room->z + (z * 1));

        v->x = mul_fixed_16_16_by_fixed_16_16(v->x, RAS_FIXED_HALF);
        v->y = mul_fixed_16_16_by_fixed_16_16(v->y, float_to_fixed_16_16(0.125));
        v->z = mul_fixed_16_16_by_fixed_16_16(v->z, RAS_FIXED_HALF);

        sector->corners[corner_index] = vert_index;
    }
}

void core_add_ceiling_pillar_corner(
    RasTombMapRoom* room,
    RasTombMapSector* sector,
    size_t corner_index,
    int32_t x,
    int32_t z)
{
    if (sector != NULL && core_tombmap_sector_is_ceiling_pillar(sector)) {

        // Pillar tip vertex
        int32_t vert_index = room->element.num_verts++;
        RasVector3f* v = &room->element.verts[vert_index].position;

        RasFixed y_offset = mul_fixed_16_16_by_fixed_16_16(
            INT_32_TO_FIXED_16_16(sector->ceiling),
            RAS_TOMBMAP_SECTOR_PILLAR_UNITS);

        v->x = INT_32_TO_FIXED_16_16(room->x + (x * 1));
        v->y = INT_32_TO_FIXED_16_16(room->y_top) + y_offset;
        v->z = INT_32_TO_FIXED_16_16(room->z + (z * 1));

        v->x = mul_fixed_16_16_by_fixed_16_16(v->x, RAS_FIXED_HALF);
        v->y = mul_fixed_16_16_by_fixed_16_16(v->y, RAS_FIXED_HALF);
        v->z = mul_fixed_16_16_by_fixed_16_16(v->z, RAS_FIXED_HALF);

        sector->corners[corner_index] = vert_index;
    }
}

RasResult core_tombmap_room_pillars_to_element_verts(
    RasTombMapRoom* room,
    RasPipelineElement* element)
{
    for (size_t z = 0; z < room->num_sectors_z + 1; z++) {
        for (size_t x = 0; x < room->num_sectors_x + 1; x++) {

            RasTombMapSector* sector_tl = core_get_sector(room, x - 1, z - 1);
            RasTombMapSector* sector_bl = core_get_sector(room, x - 1, z);
            RasTombMapSector* sector_br = core_get_sector(room, x, z);
            RasTombMapSector* sector_tr = core_get_sector(room, x, z - 1);

            core_add_floor_pillar_corner(
                room,
                sector_tl,
                FLOOR_TIP_C11,
                x,
                z);
            core_add_floor_pillar_corner(
                room,
                sector_bl,
                FLOOR_TIP_C10,
                x,
                z);
            core_add_floor_pillar_corner(
                room,
                sector_br,
                FLOOR_TIP_C00,
                x,
                z);
            core_add_floor_pillar_corner(
                room,
                sector_tr,
                FLOOR_TIP_C01,
                x,
                z);

            core_add_ceiling_pillar_corner(
                room,
                sector_tl,
                CEIL_TIP_C11,
                x,
                z);
            core_add_ceiling_pillar_corner(
                room,
                sector_bl,
                CEIL_TIP_C10,
                x,
                z);
            core_add_ceiling_pillar_corner(
                room,
                sector_br,
                CEIL_TIP_C00,
                x,
                z);
            core_add_ceiling_pillar_corner(
                room,
                sector_tr,
                CEIL_TIP_C01,
                x,
                z);
        }
    }
    return RAS_RESULT_OK;
}

/**
 * @brief Create only the room vertices.
 * The faces are created later in a pipeline step based on the camera.
 *
 * @param tombmap
 * @param element
 * @return RasResult
 */

RasResult core_tombmap_room_to_element_verts(
    RasTombMapRoom* room,
    RasPipelineElement* element)
{
    for (size_t z = 0; z < room->num_sectors_z + 1; z++) {
        for (size_t x = 0; x < room->num_sectors_x + 1; x++) {

            RasTombMapSector* sector_tl = core_get_sector(room, x - 1, z - 1);
            RasTombMapSector* sector_bl = core_get_sector(room, x - 1, z);
            RasTombMapSector* sector_br = core_get_sector(room, x, z);
            RasTombMapSector* sector_tr = core_get_sector(room, x, z - 1);

            // Floor base vertex
            int32_t vert_index = element->num_verts++;
            RasVector3f* v = &element->verts[vert_index].position;

            v->x = INT_32_TO_FIXED_16_16(room->x + (x * 1));
            v->y = INT_32_TO_FIXED_16_16(room->y_bottom); // Floor level
            v->z = INT_32_TO_FIXED_16_16(room->z + (z * 1));

            v->x = mul_fixed_16_16_by_fixed_16_16(v->x, RAS_FIXED_HALF);
            v->y = mul_fixed_16_16_by_fixed_16_16(v->y, RAS_FIXED_HALF);
            v->z = mul_fixed_16_16_by_fixed_16_16(v->z, RAS_FIXED_HALF);

            // Ceiling base vertex
            int32_t vert2_index = element->num_verts++;
            RasVector3f* v2 = &element->verts[vert2_index].position;

            v2->x = INT_32_TO_FIXED_16_16(room->x + (x * 1));
            v2->y = INT_32_TO_FIXED_16_16(room->y_top); // Ceiling level
            v2->z = INT_32_TO_FIXED_16_16(room->z + (z * 1));

            v2->x = mul_fixed_16_16_by_fixed_16_16(v2->x, RAS_FIXED_HALF);
            v2->y = mul_fixed_16_16_by_fixed_16_16(v2->y, RAS_FIXED_HALF);
            v2->z = mul_fixed_16_16_by_fixed_16_16(v2->z, RAS_FIXED_HALF);

            // Pillars and zero-height sectors start with tip and base pointing to the same vertex.
            // Walls tips point to the opposite corner vertex to simplify face creation later.
            if (sector_tl != NULL) {
                sector_tl->corners[FLOOR_BASE_C11] = vert_index;
                sector_tl->corners[FLOOR_TIP_C11] = core_tombmap_sector_is_wall(sector_tl)
                    ? vert2_index
                    : vert_index;
                sector_tl->corners[CEIL_TIP_C11] = core_tombmap_sector_is_wall(sector_tl)
                    ? vert_index
                    : vert2_index;
                sector_tl->corners[CEIL_BASE_C11] = vert2_index;
            }
            if (sector_bl != NULL) {
                sector_bl->corners[FLOOR_BASE_C10] = vert_index;
                sector_bl->corners[FLOOR_TIP_C10] = core_tombmap_sector_is_wall(sector_bl)
                    ? vert2_index
                    : vert_index;
                sector_bl->corners[CEIL_TIP_C10] = core_tombmap_sector_is_wall(sector_bl)
                    ? vert_index
                    : vert2_index;
                sector_bl->corners[CEIL_BASE_C10] = vert2_index;
            }
            if (sector_br != NULL) {
                sector_br->corners[FLOOR_BASE_C00] = vert_index;
                sector_br->corners[FLOOR_TIP_C00] = core_tombmap_sector_is_wall(sector_br)
                    ? vert2_index
                    : vert_index;
                sector_br->corners[CEIL_TIP_C00] = core_tombmap_sector_is_wall(sector_br)
                    ? vert_index
                    : vert2_index;
                sector_br->corners[CEIL_BASE_C00] = vert2_index;
            }
            if (sector_tr != NULL) {
                sector_tr->corners[FLOOR_BASE_C01] = vert_index;
                sector_tr->corners[FLOOR_TIP_C01] = core_tombmap_sector_is_wall(sector_tr)
                    ? vert2_index
                    : vert_index;
                sector_tr->corners[CEIL_TIP_C01] = core_tombmap_sector_is_wall(sector_tr)
                    ? vert_index
                    : vert2_index;
                sector_tr->corners[CEIL_BASE_C01] = vert2_index;
            }
        }
    }

    core_get_element_aabb(element, &element->aabb);
    return RAS_RESULT_OK;
}

RasResult core_tombmap_to_element_verts(
    RasSceneTombMap* tombmap)
{
    for (size_t i = 0; i < tombmap->num_rooms; i++) {
        RasTombMapRoom* room = &tombmap->rooms[i];

        RasResult result = core_tombmap_room_to_element_verts(room, &room->element);
        if (result != RAS_RESULT_OK) {
            ras_log_error("Failed to convert tombmap room to pipeline element.");
            return RAS_RESULT_ERROR;
        }
        result = core_tombmap_room_pillars_to_element_verts(room, &room->element);
        if (result != RAS_RESULT_OK) {
            ras_log_error("Failed to convert tombmap room pillars to pipeline element.");
            return RAS_RESULT_ERROR;
        }
        return RAS_RESULT_OK;
    }
}

RasResult core_tombmap_to_element_alloc(
    RasSceneTombMap* tombmap)
{
    for (size_t i = 0; i < tombmap->num_rooms; i++) {
        RasTombMapRoom* room = &tombmap->rooms[i];

        memset(&room->element, 0, sizeof(RasPipelineElement));
        room->element.max_verts = (room->num_sectors_x + 1) * (room->num_sectors_z + 1) * 4;
        room->element.verts = malloc(sizeof(RasVertex) * room->element.max_verts);
        if (room->element.verts == NULL) {
            ras_log_error("Failed to allocate memory for tombmap room verts");
            return RAS_RESULT_ERROR;
        }
        memset(room->element.verts, 0, sizeof(RasVertex) * room->element.max_verts);

        room->element.max_faces = room->num_sectors_x * room->num_sectors_z * 12;
        room->element.faces = malloc(sizeof(RasElementFace) * room->element.max_faces);
        if (room->element.faces == NULL) {
            ras_log_error("Failed to allocate memory for tombmap room faces");
            free(room->element.verts);
            return RAS_RESULT_ERROR;
        }
        memset(room->element.faces, 0, sizeof(RasElementFace) * room->element.max_faces);

        room->element.max_indexes = room->element.max_faces * 3;
        room->element.indexes = malloc(sizeof(uint32_t) * room->element.max_indexes);
        if (room->element.indexes == NULL) {
            ras_log_error("Failed to allocate memory for tombmap room indexes");
            free(room->element.verts);
            free(room->element.faces);
            return RAS_RESULT_ERROR;
        }
        memset(room->element.indexes, 0, sizeof(uint32_t) * room->element.max_indexes);

        room->element.max_material_indexes = room->element.max_faces * 3;
        room->element.material_indexes = malloc(sizeof(int32_t) * room->element.max_material_indexes);
        if (room->element.material_indexes == NULL) {
            ras_log_error("Failed to allocate memory for tombmap room material indexes");
            free(room->element.verts);
            free(room->element.faces);
            free(room->element.indexes);
            return RAS_RESULT_ERROR;
        }
    }
    return RAS_RESULT_OK;
}
