#include "mpack/src/mpack.h"
#include "rasgl/core/camera.h"
#include "rasgl/core/debug.h"
#include "rasgl/pack/pack.h"

void pack_encode_camera(mpack_writer_t* writer, RasCamera* camera)
{
    mpack_start_map(writer, 7);

    mpack_write_cstr(writer, "position");
    pack_encode_vector3f(writer, &camera->position);

    mpack_write_cstr(writer, "angle");
    mpack_write_i32(writer, camera->angle);

    mpack_write_cstr(writer, "fov");
    mpack_write_i32(writer, camera->fov);

    mpack_write_cstr(writer, "aspect_ratio");
    mpack_write_i32(writer, camera->aspect_ratio);

    mpack_write_cstr(writer, "near");
    mpack_write_i32(writer, camera->near);

    mpack_write_cstr(writer, "far");
    mpack_write_i32(writer, camera->far);

    mpack_write_cstr(writer, "projection_mode");
    mpack_write_uint(writer, camera->projection_mode);

    mpack_finish_map(writer);
}
