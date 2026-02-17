#include "mpack/src/mpack.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/tombmap.h"
#include "rasgl/pack/pack.h"

void pack_encode_tombmap(mpack_writer_t* writer, RasSceneTombMap* tombmap)
{
    mpack_start_map(writer, 3);

    mpack_write_cstr(writer, "name");
    mpack_write_cstr(writer, tombmap->name);

    mpack_write_cstr(writer, "num_rooms");
    mpack_write_uint(writer, tombmap->num_rooms);

    mpack_write_cstr(writer, "rooms");
    mpack_start_array(writer, tombmap->num_rooms);
    for (size_t r = 0; r < tombmap->num_rooms; r++) {
        RasTombMapRoom* room = &tombmap->rooms[r];
        mpack_start_map(writer, 10);
        mpack_write_cstr(writer, "x");
        mpack_write_i32(writer, room->x);
        mpack_write_cstr(writer, "z");
        mpack_write_i32(writer, room->z);
        mpack_write_cstr(writer, "y_top");
        mpack_write_i32(writer, room->y_top);
        mpack_write_cstr(writer, "y_bottom");
        mpack_write_i32(writer, room->y_bottom);
        mpack_write_cstr(writer, "num_sectors_x");
        mpack_write_uint(writer, room->num_sectors_x);
        mpack_write_cstr(writer, "num_sectors_z");
        mpack_write_uint(writer, room->num_sectors_z);
        mpack_write_cstr(writer, "mesh_index");
        mpack_write_u32(writer, room->mesh_index);

        /* sectors */
        size_t num_sectors = room->num_sectors_x * room->num_sectors_z;
        mpack_write_cstr(writer, "num_sectors");
        mpack_write_uint(writer, num_sectors);
        mpack_write_cstr(writer, "sectors");
        mpack_start_array(writer, num_sectors);
        for (size_t s = 0; s < num_sectors; s++) {
            RasTombMapSector* sec = &room->sectors[s];
            mpack_start_map(writer, 6);
            mpack_write_cstr(writer, "material");
            mpack_write_i32(writer, sec->material);
            mpack_write_cstr(writer, "spatial_flags");
            mpack_write_u8(writer, sec->spatial_flags);
            mpack_write_cstr(writer, "spatial_materials");
            mpack_start_array(writer, 4);
            for (size_t si = 0; si < 4; si++) {
                mpack_write_i32(writer, sec->spatial_materials[si]);
            }
            mpack_finish_array(writer);
            mpack_write_cstr(writer, "ceiling");
            mpack_write_i8(writer, sec->ceiling);
            mpack_write_cstr(writer, "floor");
            mpack_write_i8(writer, sec->floor);
            mpack_write_cstr(writer, "corners");
            mpack_start_array(writer, RAS_TOMBMAP_SECTOR_VERTS_MAX);
            for (size_t ci = 0; ci < RAS_TOMBMAP_SECTOR_VERTS_MAX; ci++) {
                mpack_write_i32(writer, sec->corners[ci]);
            }
            mpack_finish_array(writer);
            mpack_finish_map(writer);
        }
        mpack_finish_array(writer); /* sectors */

        /* element */
        mpack_write_cstr(writer, "element");
        pack_encode_element(writer, &room->element);

        mpack_finish_map(writer); /* room */
    }
    mpack_finish_array(writer); /* rooms */

    mpack_finish_map(writer);
}
