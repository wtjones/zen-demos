#include "rasgl/pack/pack.h"
#include <stdlib.h>

char* pack_encode_scene(RasScene* scene, size_t* out_size)
{
    char* data;
    mpack_writer_t writer;
    mpack_writer_init_growable(&writer, &data, out_size);

    mpack_start_map(&writer, 5);

    mpack_write_cstr(&writer, "name");
    mpack_write_cstr(&writer, scene->name);
    mpack_write_cstr(&writer, "num_models");
    mpack_write_uint(&writer, scene->num_models);
    mpack_write_cstr(&writer, "models");
    mpack_start_array(&writer, scene->num_models);
    for (size_t i = 0; i < scene->num_models; i++) {
        RasSceneModel* model = &scene->models[i];
        pack_encode_model(&writer, model);
    }
    ras_log_info("Finished encoding models");
    mpack_finish_array(&writer);

    mpack_write_cstr(&writer, "num_objects");
    mpack_write_uint(&writer, scene->num_objects);
    mpack_write_cstr(&writer, "objects");
    mpack_start_array(&writer, scene->num_objects);
    for (size_t i = 0; i < scene->num_objects; i++) {
        pack_encode_object(&writer, &scene->objects[i]);
    }
    mpack_finish_array(&writer); // objects

    mpack_write_cstr(&writer, "num_cameras");
    mpack_write_uint(&writer, scene->num_cameras);
    mpack_write_cstr(&writer, "cameras");
    mpack_start_array(&writer, scene->num_cameras);
    for (size_t i = 0; i < scene->num_cameras; i++) {
        pack_encode_camera(&writer, &scene->cameras[i]);
    }
    mpack_finish_array(&writer); // cameras

    mpack_finish_map(&writer);

    if (mpack_writer_destroy(&writer) != mpack_ok) {
        ras_log_error("An error occurred encoding the data!\n");
        return NULL;
    }

    return data;
}
