#include "rasgl/core/scene.h"
#include "rasgl/pack/pack.h"
#include <stdlib.h>

RasScene* pack_decode_scene(const char* data, size_t size)
{

    mpack_tree_t tree;
    RasScene* scene = malloc(sizeof(RasScene));
    if (scene == NULL) {
        ras_log_error("Failed to allocate memory for scene.");
        return NULL;
    }
    memset(scene, 0, sizeof(RasScene));
    mpack_tree_init(&tree, data, size);
    mpack_tree_parse(&tree);

    mpack_node_t root = mpack_tree_root(&tree);

    mpack_node_copy_cstr(mpack_node_map_cstr(root, "name"), scene->name, MAX_SCENE_NAME);
    scene->num_models = mpack_node_i32(mpack_node_map_cstr(root, "num_models"));

    if (scene->num_models > 0) {
        scene->models = malloc(sizeof(RasSceneModel) * scene->num_models);

        if (scene->models == NULL) {
            ras_log_error("Failed to allocate memory for scene models.");
            mpack_tree_destroy(&tree);
            free(scene);
            return NULL;
        }

        mpack_node_t models_node = mpack_node_map_cstr(root, "models");

        for (size_t i = 0; i < scene->num_models; i++) {
            mpack_node_t model_node = mpack_node_array_at(models_node, i);
            if (pack_decode_scene_model(model_node, &scene->models[i]) != RAS_RESULT_OK) {
                ras_log_error("Failed to decode scene model %zu.", i);
                mpack_tree_destroy(&tree);
                free(scene->models);
                free(scene);
                return NULL;
            }
        }
    }

    scene->num_objects = mpack_node_i32(mpack_node_map_cstr(root, "num_objects"));
    // TODO decode objects

    // clean up and check for errors
    if (mpack_tree_destroy(&tree) != mpack_ok) {
        ras_log_error("An error occurred decoding scene with size %zu.", size);
        free(scene->models);
        free(scene);
        return NULL;
    }

    return scene;
}
