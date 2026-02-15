#include "mpack/src/mpack.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/model.h"
#include "rasgl/pack/pack.h"
#include <stdlib.h>

void pack_encode_element(mpack_writer_t* writer, RasPipelineElement* element)
{
    mpack_start_map(writer, 7);

    mpack_write_cstr(writer, "num_verts");
    mpack_write_uint(writer, element->num_verts);

    mpack_write_cstr(writer, "verts");
    mpack_start_array(writer, element->num_verts);
    for (size_t i = 0; i < element->num_verts; i++) {
        pack_encode_vertex(writer, &element->verts[i]);
    }
    mpack_finish_array(writer);

    mpack_write_cstr(writer, "num_faces");
    mpack_write_uint(writer, element->num_faces);
    mpack_write_cstr(writer, "num_indexes");
    mpack_write_uint(writer, element->num_indexes);
    mpack_write_cstr(writer, "max_indexes");
    mpack_write_uint(writer, element->max_indexes);
    mpack_write_cstr(writer, "num_material_indexes");
    mpack_write_uint(writer, element->num_material_indexes);
    // TODO faces, indexes, material indexes
    mpack_write_cstr(writer, "aabb");
    pack_encode_aabb(writer, &element->aabb);

    mpack_finish_map(writer);
}
