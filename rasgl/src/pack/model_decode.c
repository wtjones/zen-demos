#include "rasgl/pack/pack.h"
#include <stdlib.h>

RasResult pack_decode_scene_model(mpack_node_t* node, RasSceneModel* model)
{
    mpack_node_copy_cstr(mpack_node_map_cstr(*node, "name"), model->name, MAX_SCENE_NAME);
    ras_log_info("Decoded model name: %s", model->name);
    mpack_node_copy_cstr(mpack_node_map_cstr(*node, "path"), model->path, MAX_FILE_PATH);
    ras_log_info("Decoded model path: %s", model->path);
    // TODO decode element
    return RAS_RESULT_OK;
}
