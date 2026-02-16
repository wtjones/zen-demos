#include "rasgl/pack/pack.h"

RasResult pack_decode_face(mpack_node_t node, RasPipelineFace* face)
{
    if (pack_decode_vector3f(mpack_node_map_cstr(node, "normal"), &face->normal) != RAS_RESULT_OK) {
        ras_log_error("Invalid face normal data");
        return RAS_RESULT_ERROR;
    }
    if (pack_decode_vector3f(mpack_node_map_cstr(node, "view_space_normal"), &face->view_space_normal) != RAS_RESULT_OK) {
        ras_log_error("Invalid face view_space_normal data");
        return RAS_RESULT_ERROR;
    }

    face->material_index = mpack_node_i32(mpack_node_map_cstr(node, "material_index"));
    face->diffuse_intensity = mpack_node_i32(mpack_node_map_cstr(node, "diffuse_intensity"));
    face->clip_flags = (RasClipFlags)mpack_node_u32(mpack_node_map_cstr(node, "clip_flags"));
    face->outline_edges = (uint8_t)mpack_node_u8(mpack_node_map_cstr(node, "outline_edges"));
    face->outline_material_index = mpack_node_i32(mpack_node_map_cstr(node, "outline_material_index"));

    return RAS_RESULT_OK;
}
