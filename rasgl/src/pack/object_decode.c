#include "rasgl/core/scene.h"
#include "rasgl/pack/pack.h"
#include <stdlib.h>

RasResult pack_decode_scene_object(mpack_node_t node, RasSceneObject* obj)
{
    memset(obj, 0, sizeof(RasSceneObject));

    obj->model_index = mpack_node_i32(mpack_node_map_cstr(node, "model_index"));

    mpack_node_t pos_node = mpack_node_map_cstr(node, "position");
    if (pack_decode_vector3f(pos_node, &obj->position) != RAS_RESULT_OK) {
        ras_log_error("Invalid object position data");
        return RAS_RESULT_ERROR;
    }

    mpack_node_t rot_node = mpack_node_map_cstr(node, "rotation");
    if (pack_decode_vector3f(rot_node, &obj->rotation) != RAS_RESULT_OK) {
        ras_log_error("Invalid object rotation data");
        return RAS_RESULT_ERROR;
    }

    mpack_node_t rot_delta_node = mpack_node_map_cstr(node, "rotation_delta");
    if (pack_decode_vector3f(rot_delta_node, &obj->rotation_delta) != RAS_RESULT_OK) {
        ras_log_error("Invalid object rotation_delta data");
        return RAS_RESULT_ERROR;
    }

    obj->num_animations = mpack_node_i32(mpack_node_map_cstr(node, "num_animations"));
    if (obj->num_animations > 0) {
        obj->animations = malloc(sizeof(RasSceneObjectAnimation) * obj->num_animations);
        if (obj->animations == NULL) {
            ras_log_error("Failed to allocate memory for object animations.");
            return RAS_RESULT_ERROR;
        }
        mpack_node_t animations_node = mpack_node_map_cstr(node, "animations");
        for (size_t i = 0; i < obj->num_animations; i++) {
            mpack_node_t anim_node = mpack_node_array_at(animations_node, i);
            mpack_node_t rotation_node = mpack_node_map_cstr(anim_node, "rotation");
            if (pack_decode_vector3f(mpack_node_map_cstr(rotation_node, "axis"), &obj->animations[i].rotation.axis) != RAS_RESULT_OK) {
                ras_log_error("Invalid animation axis data");
                free(obj->animations);
                obj->animations = NULL;
                obj->num_animations = 0;
                return RAS_RESULT_ERROR;
            }
            obj->animations[i].rotation.speed = mpack_node_i32(mpack_node_map_cstr(rotation_node, "speed"));
        }
    } else {
        obj->animations = NULL;
    }

    obj->mesh_index = (uint32_t)mpack_node_u32(mpack_node_map_cstr(node, "mesh_index"));

    return RAS_RESULT_OK;
}
