#include "mpack/src/mpack.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/scene.h"
#include "rasgl/pack/pack.h"
#include <stdlib.h>

void pack_encode_object(mpack_writer_t* writer, RasSceneObject* obj)
{
    mpack_start_map(writer, 7);

    mpack_write_cstr(writer, "model_index");
    mpack_write_i32(writer, obj->model_index);

    mpack_write_cstr(writer, "position");
    pack_encode_vector3f(writer, &obj->position);

    mpack_write_cstr(writer, "rotation");
    pack_encode_vector3f(writer, &obj->rotation);

    mpack_write_cstr(writer, "rotation_delta");
    pack_encode_vector3f(writer, &obj->rotation_delta);

    mpack_write_cstr(writer, "num_animations");
    mpack_write_uint(writer, obj->num_animations);

    mpack_write_cstr(writer, "animations");
    mpack_start_array(writer, obj->num_animations);
    for (size_t i = 0; i < obj->num_animations; i++) {
        RasSceneObjectAnimation* anim = &obj->animations[i];
        mpack_start_map(writer, 1);
        mpack_write_cstr(writer, "rotation");
        mpack_start_map(writer, 2);
        mpack_write_cstr(writer, "axis");
        pack_encode_vector3f(writer, &anim->rotation.axis);
        mpack_write_cstr(writer, "speed");
        mpack_write_i32(writer, anim->rotation.speed);
        mpack_finish_map(writer); /* rotation */
        mpack_finish_map(writer); /* animation */
    }
    mpack_finish_array(writer); /* animations */

    mpack_write_cstr(writer, "mesh_index");
    mpack_write_u32(writer, obj->mesh_index);

    mpack_finish_map(writer);
}
