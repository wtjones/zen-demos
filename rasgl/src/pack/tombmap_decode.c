#include "rasgl/pack/pack.h"
#include <stdlib.h>
#include <string.h>

RasResult pack_decode_tombmap(mpack_node_t node, RasSceneTombMap* tombmap)
{
    // Use 'commit-on-success' pattern to avoid having to free partially
    // decoded data on failure.
    // See also libglt2d's gltf parsing for a similar approach.
    RasSceneTombMap tmp;
    memset(&tmp, 0, sizeof(RasSceneTombMap));

    mpack_node_copy_cstr(mpack_node_map_cstr(node, "name"), tmp.name, MAX_TOMBMAP_NAME);

    tmp.num_rooms = mpack_node_u32(mpack_node_map_cstr(node, "num_rooms"));
    if (tmp.num_rooms > 0) {
        tmp.rooms = malloc(sizeof(RasTombMapRoom) * tmp.num_rooms);
        if (tmp.rooms == NULL) {
            ras_log_error("Failed to allocate tombmap rooms");
            return RAS_RESULT_ERROR;
        }
        memset(tmp.rooms, 0, sizeof(RasTombMapRoom) * tmp.num_rooms);

        mpack_node_t rooms_node = mpack_node_map_cstr(node, "rooms");
        for (size_t r = 0; r < tmp.num_rooms; r++) {
            mpack_node_t room_node = mpack_node_array_at(rooms_node, r);
            RasTombMapRoom* room = &tmp.rooms[r];
            room->x = mpack_node_i32(mpack_node_map_cstr(room_node, "x"));
            room->z = mpack_node_i32(mpack_node_map_cstr(room_node, "z"));
            room->y_top = mpack_node_i32(mpack_node_map_cstr(room_node, "y_top"));
            room->y_bottom = mpack_node_i32(mpack_node_map_cstr(room_node, "y_bottom"));
            room->num_sectors_x = mpack_node_u32(mpack_node_map_cstr(room_node, "num_sectors_x"));
            room->num_sectors_z = mpack_node_u32(mpack_node_map_cstr(room_node, "num_sectors_z"));
            room->mesh_index = (uint32_t)mpack_node_u32(mpack_node_map_cstr(room_node, "mesh_index"));

            size_t num_sectors = mpack_node_u32(mpack_node_map_cstr(room_node, "num_sectors"));
            if (num_sectors > 0) {
                room->sectors = malloc(sizeof(RasTombMapSector) * num_sectors);
                if (room->sectors == NULL) {
                    ras_log_error("Failed to allocate tombmap sectors");
                    goto fail;
                }
                memset(room->sectors, 0, sizeof(RasTombMapSector) * num_sectors);

                mpack_node_t sectors_node = mpack_node_map_cstr(room_node, "sectors");
                for (size_t s = 0; s < num_sectors; s++) {
                    mpack_node_t sec_node = mpack_node_array_at(sectors_node, s);
                    RasTombMapSector* sec = &room->sectors[s];
                    sec->material = mpack_node_i32(mpack_node_map_cstr(sec_node, "material"));
                    sec->spatial_flags = (uint8_t)mpack_node_u8(mpack_node_map_cstr(sec_node, "spatial_flags"));
                    mpack_node_t sm_node = mpack_node_map_cstr(sec_node, "spatial_materials");
                    for (size_t si = 0; si < 4; si++) {
                        sec->spatial_materials[si] = mpack_node_i32(mpack_node_array_at(sm_node, si));
                    }
                    sec->ceiling = (int8_t)mpack_node_i8(mpack_node_map_cstr(sec_node, "ceiling"));
                    sec->floor = (int8_t)mpack_node_i8(mpack_node_map_cstr(sec_node, "floor"));
                    mpack_node_t corners_node = mpack_node_map_cstr(sec_node, "corners");
                    for (size_t ci = 0; ci < RAS_TOMBMAP_SECTOR_VERTS_MAX; ci++) {
                        sec->corners[ci] = mpack_node_i32(mpack_node_array_at(corners_node, ci));
                    }
                }
            }

            /* element */
            mpack_node_t element_node = mpack_node_map_cstr(room_node, "element");
            memset(&room->element, 0, sizeof(RasPipelineElement));
            if (pack_decode_element(element_node, &room->element) != RAS_RESULT_OK) {
                ras_log_error("Failed to decode tombmap room element");
                goto fail;
            }
        }
    }

    /* commit */
    *tombmap = tmp;
    return RAS_RESULT_OK;

fail:
    if (tmp.rooms) {
        for (size_t rr = 0; rr < tmp.num_rooms; rr++) {
            RasTombMapRoom* r = &tmp.rooms[rr];
            if (r->sectors)
                free(r->sectors);
            core_pipeline_element_free(&r->element);
        }
        free(tmp.rooms);
    }
    return RAS_RESULT_ERROR;
}
