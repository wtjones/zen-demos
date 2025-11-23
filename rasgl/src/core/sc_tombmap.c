
#include "rasgl/core/graphics.h"
#include "rasgl/core/tombmap.h"

/**
 * @brief Create only the room vertices.
 * The faces are created during the pipeline based on the camera.
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
            RasVector3f* v = &element->verts[element->num_verts].position;
            element->num_verts++;

            // FIXME: Add verts for floor and ceiling levels.
            v->x = INT_32_TO_FIXED_16_16(room->x + (x * RAS_TOMBMAP_SECTOR_UNITS));
            v->y = INT_32_TO_FIXED_16_16(room->y_bottom); // Floor level
            v->z = INT_32_TO_FIXED_16_16(room->z + (z * RAS_TOMBMAP_SECTOR_UNITS));
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
