
#include "rasgl/core/graphics.h"
#include "rasgl/core/tombmap.h"

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
    element->num_faces++;
}

RasResult core_tombmap_room_to_element_faces(
    RasTombMapRoom* room,
    RasCamera* camera,
    RasPipelineElement* element)
{

    if (room->num_sectors_x * room->num_sectors_z * 8 > MAX_PIPELINE_FACES) {
        ras_log_error("Reached max faces in pipeline element: %d", MAX_PIPELINE_FACES);
        assert(false);
    }

    if (room->num_sectors_x * room->num_sectors_z * 24 > MAX_VISIBLE_INDEXES) {
        ras_log_error("Reached max indexes in pipeline element: %d", MAX_VISIBLE_INDEXES);
        assert(false);
    }

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
                ? sector->material
                : (sector->material + 3);
            add_sector_face(
                element,
                sector->corners[FLOOR_BASE_C10],
                sector->corners[FLOOR_BASE_C00],
                sector->corners[FLOOR_BASE_C01],
                checker);
            add_sector_face(
                element,
                sector->corners[FLOOR_BASE_C10],
                sector->corners[FLOOR_BASE_C01],
                sector->corners[FLOOR_BASE_C11],
                checker);

            // Create interior ceiling face in CCW order.
            // Looking up:
            // C01 -- C11
            // |       |
            // |       |
            // C00 -- C10
            // Order: (C11, C01, C00), (C11, C00, C10)

            add_sector_face(
                element,
                sector->corners[CEIL_BASE_C11],
                sector->corners[CEIL_BASE_C01],
                sector->corners[CEIL_BASE_C00],
                checker);
            add_sector_face(
                element,
                sector->corners[CEIL_BASE_C11],
                sector->corners[CEIL_BASE_C00],
                sector->corners[CEIL_BASE_C10],
                checker);

            if (core_tombmap_sector_is_wall(sector_z_minus)) {

                // Create interior far face in CCW order.
                // Looking far:
                // CT00 -- CT10
                // |       |
                // |       |
                // FT00 -- FT10
                // Order: (CT10, CT00, FT00), (CT01, FT00, FT10)

                add_sector_face(
                    element,
                    sector->corners[CEIL_BASE_C10],
                    sector->corners[CEIL_BASE_C00],
                    sector->corners[FLOOR_BASE_C00],
                    sector_z_minus->material);
                add_sector_face(
                    element,
                    sector->corners[CEIL_BASE_C10],
                    sector->corners[FLOOR_BASE_C00],
                    sector->corners[FLOOR_BASE_C10],
                    sector_z_minus->material);
            }

            if (core_tombmap_sector_is_wall(sector_x_minus)) {

                // Create interior left face in CCW order.
                // Looking left:
                // CT01 -- CT00
                // |          |
                // |          |
                // FT01 -- FT00
                // Order: (CT00, CT01, FT01), (CT00, FT01, FT00)

                add_sector_face(
                    element,
                    sector->corners[CEIL_BASE_C00],
                    sector->corners[CEIL_BASE_C01],
                    sector->corners[FLOOR_BASE_C01],
                    sector_x_minus->material);

                add_sector_face(
                    element,
                    sector->corners[CEIL_BASE_C00],
                    sector->corners[FLOOR_BASE_C01],
                    sector->corners[FLOOR_BASE_C00],
                    sector_x_minus->material);
            }

            if (core_tombmap_sector_is_wall(sector_z_plus)) {

                // Create interior near face in CCW order.
                // Looking near:
                // CT11 -- CT01
                // |       |
                // |       |
                // FT11 -- FT01
                // Order: (CT01, CT11, FT11), (CT01, FT11, FT01)

                add_sector_face(
                    element,
                    sector->corners[CEIL_BASE_C01],
                    sector->corners[CEIL_BASE_C11],
                    sector->corners[FLOOR_BASE_C11],
                    sector_z_plus->material);

                add_sector_face(
                    element,
                    sector->corners[CEIL_BASE_C01],
                    sector->corners[FLOOR_BASE_C11],
                    sector->corners[FLOOR_BASE_C01],
                    sector_z_plus->material);
            }

            if (core_tombmap_sector_is_wall(sector_x_plus)) {

                // Create interior right face in CCW order.
                // Looking right:
                // CT10 -- CT11
                // |       |
                // |       |
                // FT10 -- FT11
                // Order: (CT11, CT10, FT10), (CT11, FT10, FT11)

                add_sector_face(
                    element,
                    sector->corners[CEIL_BASE_C11],
                    sector->corners[CEIL_BASE_C10],
                    sector->corners[FLOOR_BASE_C10],
                    sector_x_plus->material);
                add_sector_face(
                    element,
                    sector->corners[CEIL_BASE_C11],
                    sector->corners[FLOOR_BASE_C10],
                    sector->corners[FLOOR_BASE_C11],
                    sector_x_plus->material);
            }
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
    memset(element, 0, sizeof(RasPipelineElement));

    for (size_t z = 0; z < room->num_sectors_z + 1; z++) {
        for (size_t x = 0; x < room->num_sectors_x + 1; x++) {

            RasTombMapSector* sector_tl = core_get_sector(room, x - 1, z - 1);
            RasTombMapSector* sector_bl = core_get_sector(room, x - 1, z);
            RasTombMapSector* sector_br = core_get_sector(room, x, z);
            RasTombMapSector* sector_tr = core_get_sector(room, x, z - 1);
            int32_t vert_index = element->num_verts++;
            RasVector3f* v = &element->verts[vert_index].position;

            v->x = INT_32_TO_FIXED_16_16(room->x + (x * 1));
            v->y = INT_32_TO_FIXED_16_16(room->y_bottom); // Floor level
            v->z = INT_32_TO_FIXED_16_16(room->z + (z * 1));

            v->x = mul_fixed_16_16_by_fixed_16_16(v->x, RAS_FIXED_HALF);
            v->y = mul_fixed_16_16_by_fixed_16_16(v->y, RAS_FIXED_HALF);
            v->z = mul_fixed_16_16_by_fixed_16_16(v->z, RAS_FIXED_HALF);

            int32_t vert2_index = element->num_verts++;
            RasVector3f* v2 = &element->verts[vert2_index].position;

            v2->x = INT_32_TO_FIXED_16_16(room->x + (x * 1));
            v2->y = INT_32_TO_FIXED_16_16(room->y_top); // Ceiling level
            v2->z = INT_32_TO_FIXED_16_16(room->z + (z * 1));

            v2->x = mul_fixed_16_16_by_fixed_16_16(v2->x, RAS_FIXED_HALF);
            v2->y = mul_fixed_16_16_by_fixed_16_16(v2->y, RAS_FIXED_HALF);
            v2->z = mul_fixed_16_16_by_fixed_16_16(v2->z, RAS_FIXED_HALF);

            if (sector_tl != NULL) {
                sector_tl->corners[FLOOR_BASE_C11] = vert_index;
                sector_tl->corners[FLOOR_TIP_C11] = vert_index;
                sector_tl->corners[CEIL_BASE_C11] = vert2_index;
                sector_tl->corners[CEIL_TIP_C11] = vert2_index;
            }
            if (sector_bl != NULL) {
                sector_bl->corners[FLOOR_BASE_C10] = vert_index;
                sector_bl->corners[FLOOR_TIP_C10] = vert_index;
                sector_bl->corners[CEIL_BASE_C10] = vert2_index;
                sector_bl->corners[CEIL_TIP_C10] = vert2_index;
            }
            if (sector_br != NULL) {
                sector_br->corners[FLOOR_BASE_C00] = vert_index;
                sector_br->corners[FLOOR_TIP_C00] = vert_index;
                sector_br->corners[CEIL_BASE_C00] = vert2_index;
                sector_br->corners[CEIL_TIP_C00] = vert2_index;
            }
            if (sector_tr != NULL) {
                sector_tr->corners[FLOOR_BASE_C01] = vert_index;
                sector_tr->corners[FLOOR_TIP_C01] = vert_index;
                sector_tr->corners[CEIL_BASE_C01] = vert2_index;
                sector_tr->corners[CEIL_TIP_C01] = vert2_index;
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
    }
    return RAS_RESULT_OK;
}
