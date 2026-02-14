#include "mpack/src/mpack.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/model.h"
#include "rasgl/pack/pack.h"
#include <stdlib.h>

void pack_encode_element(mpack_writer_t* writer, RasPipelineElement* element)
{
    mpack_start_map(writer, 5);

    mpack_write_cstr(writer, "num_verts");
    mpack_write_uint(writer, element->num_verts);
    mpack_write_cstr(writer, "num_faces");
    mpack_write_uint(writer, element->num_faces);
    mpack_write_cstr(writer, "num_indexes");
    mpack_write_uint(writer, element->num_indexes);
    mpack_write_cstr(writer, "num_material_indexes");
    mpack_write_uint(writer, element->num_material_indexes);
    mpack_finish_map(writer);
}
