#include "rasgl/pack/pack.h"
#include <stdlib.h>

RasResult pack_decode_camera(mpack_node_t node, RasCamera* camera)
{
    mpack_node_t pos_node = mpack_node_map_cstr(node, "position");
    if (pack_decode_vector3f(pos_node, &camera->position) != RAS_RESULT_OK) {
        ras_log_error("Invalid camera position");
        return RAS_RESULT_ERROR;
    }

    camera->angle = mpack_node_i32(mpack_node_map_cstr(node, "angle"));
    camera->fov = mpack_node_i32(mpack_node_map_cstr(node, "fov"));
    camera->aspect_ratio = mpack_node_i32(mpack_node_map_cstr(node, "aspect_ratio"));
    camera->near = mpack_node_i32(mpack_node_map_cstr(node, "near"));
    camera->far = mpack_node_i32(mpack_node_map_cstr(node, "far"));
    camera->projection_mode = (RasProjectionMode)mpack_node_u32(mpack_node_map_cstr(node, "projection_mode"));

    return RAS_RESULT_OK;
}
