#include "mpack/src/mpack.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/model.h"
#include "rasgl/pack/pack.h"

void pack_encode_face(mpack_writer_t* writer, RasElementFace* face)
{
    mpack_start_map(writer, 4);

    mpack_write_cstr(writer, "normal");
    pack_encode_vector3f(writer, &face->normal);

    mpack_write_cstr(writer, "material_index");
    mpack_write_int(writer, face->material_index);

    mpack_write_cstr(writer, "outline_edges");
    mpack_write_u8(writer, face->outline_edges);

    mpack_write_cstr(writer, "outline_material_index");
    mpack_write_int(writer, face->outline_material_index);

    mpack_finish_map(writer);
}
