#include "rasgl/pack/pack.h"

RasResult pack_decode_face(mpack_node_t node, RasElementFace* face)
{
    if (pack_decode_vector3f(mpack_node_map_cstr(node, "normal"), &face->normal) != RAS_RESULT_OK) {
        ras_log_error("Invalid face normal data");
        return RAS_RESULT_ERROR;
    }

    face->material_index = mpack_node_i32(mpack_node_map_cstr(node, "material_index"));
    face->outline_edges = (uint8_t)mpack_node_u8(mpack_node_map_cstr(node, "outline_edges"));
    face->outline_material_index = mpack_node_i32(mpack_node_map_cstr(node, "outline_material_index"));

    return RAS_RESULT_OK;
}
