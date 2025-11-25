
#include "rasgl/core/graphics.h"
#include "rasgl/core/tombmap.h"

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

            RasTombMapSector* sector_tl = z == 0 || x == 0
                ? NULL
                : &room->sectors[((z - 1) * room->num_sectors_z) + x - 1];
            RasTombMapSector* sector_bl = z == room->num_sectors_z || x == 0
                ? NULL
                : &room->sectors[((z)*room->num_sectors_z) + x - 1];
            RasTombMapSector* sector_br = z == room->num_sectors_z || x == room->num_sectors_x
                ? NULL
                : &room->sectors[((z)*room->num_sectors_z) + (x)];
            RasTombMapSector* sector_tr = z == 0 || x == room->num_sectors_x
                ? NULL
                : &room->sectors[((z - 1) * room->num_sectors_z) + (x)];

            int32_t vert_index = element->num_verts++;
            RasVector3f* v = &element->verts[vert_index].position;

            v->x = INT_32_TO_FIXED_16_16(room->x + (x * 1));
            v->y = INT_32_TO_FIXED_16_16(room->y_bottom); // Floor level
            v->z = INT_32_TO_FIXED_16_16(room->z + (z * 1));

            int32_t vert2_index = element->num_verts++;
            RasVector3f* v2 = &element->verts[vert2_index].position;

            v2->x = INT_32_TO_FIXED_16_16(room->x + (x * 1));
            v2->y = INT_32_TO_FIXED_16_16(room->y_top); // Ceiling level
            v2->z = INT_32_TO_FIXED_16_16(room->z + (z * 1));

            if (sector_tl != NULL) {
                sector_tl->corners[FLOOR_BASE_C00] = vert_index;
                sector_tl->corners[FLOOR_TIP_C00] = vert_index;
                sector_tl->corners[CEIL_BASE_C00] = vert2_index;
                sector_tl->corners[CEIL_TIP_C00] = vert2_index;
            }
            if (sector_bl != NULL) {
                sector_bl->corners[FLOOR_BASE_C01] = vert_index;
                sector_bl->corners[FLOOR_TIP_C01] = vert_index;
                sector_bl->corners[CEIL_BASE_C01] = vert2_index;
                sector_bl->corners[CEIL_TIP_C01] = vert2_index;
            }
            if (sector_br != NULL) {
                sector_br->corners[FLOOR_BASE_C11] = vert_index;
                sector_br->corners[FLOOR_TIP_C11] = vert_index;
                sector_br->corners[CEIL_BASE_C11] = vert2_index;
                sector_br->corners[CEIL_TIP_C11] = vert2_index;
            }
            if (sector_tr != NULL) {
                sector_tr->corners[FLOOR_BASE_C10] = vert_index;
                sector_tr->corners[FLOOR_TIP_C10] = vert_index;
                sector_tr->corners[CEIL_BASE_C10] = vert2_index;
                sector_tr->corners[CEIL_TIP_C10] = vert2_index;
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
