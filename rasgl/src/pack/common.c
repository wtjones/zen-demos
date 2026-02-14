#include "mpack/src/mpack.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/model.h"
#include "rasgl/pack/pack.h"
#include <stdlib.h>

void pack_encode_vertex(mpack_writer_t* writer, RasVertex* vertex)
{
    mpack_start_map(writer, 5);

    mpack_write_cstr(writer, "position");
    mpack_write_array(writer, 3);
    mpack_write_fixed(writer, vertex->position.x);
    mpack_write_fixed(writer, vertex->position.y);
    mpack_write_fixed(writer, vertex->position.z);

    mpack_write_cstr(writer, "color");
    mpack_write_u8(writer, vertex->color);

    mpack_write_cstr(writer, "u");
    mpack_write_fixed(writer, vertex->u);

    mpack_write_cstr(writer, "v");
    mpack_write_fixed(writer, vertex->v);

    mpack_write_cstr(writer, "normal");
    mpack_write_array(writer, 3);
    mpack_write_fixed(writer, vertex->normal.x);
    mpack_write_fixed(writer, vertex->normal.y);
    mpack_write_fixed(writer, vertex->normal.z);

    mpack_finish_map(writer);
}

void pack_encode_face(mpack_writer_t* writer, RasPipelineFace* face)
{
    mpack_start_map(writer, 6);

    mpack_write_cstr(writer, "normal");
    mpack_write_array(writer, 3);
    mpack_write_fixed(writer, face->normal.x);
    mpack_write_fixed(writer, face->normal.y);
    mpack_write_fixed(writer, face->normal.z);

    mpack_write_cstr(writer, "view_space_normal");
    mpack_write_array(writer, 3);
    mpack_write_fixed(writer, face->view_space_normal.x);
    mpack_write_fixed(writer, face->view_space_normal.y);
    mpack_write_fixed(writer, face->view_space_normal.z);

    mpack_write_cstr(writer, "material_index");
    mpack_write_int(writer, face->material_index);

    mpack_write_cstr(writer, "diffuse_intensity");
    mpack_write_fixed(writer, face->diffuse_intensity);

    mpack_write_cstr(writer, "clip_flags");
    mpack_write_uint(writer, face->clip_flags);

    mpack_write_cstr(writer, "outline_edges");
    mpack_write_u8(writer, face->outline_edges);

    mpack_write_cstr(writer, "outline_material_index");
    mpack_write_int(writer, face->outline_material_index);

    mpack_finish_map(writer);
}
void pack_encode_aabb(mpack_writer_t* writer, RasAABB* aabb)
{
    mpack_start_map(writer, 2);

    mpack_write_cstr(writer, "min");
    mpack_write_array(writer, 3);
    mpack_write_fixed(writer, aabb->min.x);
    mpack_write_fixed(writer, aabb->min.y);
    mpack_write_fixed(writer, aabb->min.z);

    mpack_write_cstr(writer, "max");
    mpack_write_array(writer, 3);
    mpack_write_fixed(writer, aabb->max.x);
    mpack_write_fixed(writer, aabb->max.y);
    mpack_write_fixed(writer, aabb->max.z);

    mpack_finish_map(writer);
}
