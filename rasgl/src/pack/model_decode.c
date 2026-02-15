#include "rasgl/pack/pack.h"
#include <stdlib.h>

RasResult pack_decode_scene_model(mpack_node_t node, RasSceneModel* model)
{
    mpack_node_copy_cstr(mpack_node_map_cstr(node, "name"), model->name, MAX_SCENE_NAME);
    ras_log_info("Decoded model name: %s", model->name);
    mpack_node_copy_cstr(mpack_node_map_cstr(node, "path"), model->path, MAX_FILE_PATH);
    ras_log_info("Decoded model path: %s", model->path);

    mpack_node_t element_node = mpack_node_map_cstr(node, "element");
    if (pack_decode_element(element_node, &model->element) != RAS_RESULT_OK) {
        ras_log_error("Failed to decode model element for model %s", model->name);
        return RAS_RESULT_ERROR;
    }

    return RAS_RESULT_OK;
}
