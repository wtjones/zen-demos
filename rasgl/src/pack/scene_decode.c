#include "rasgl/core/scene.h"
#include "rasgl/pack/pack.h"
#include <stdlib.h>
#include <string.h>

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

    size_t expected_objects = mpack_node_i32(mpack_node_map_cstr(root, "num_objects"));
    if (expected_objects > 0) {
        scene->objects = malloc(sizeof(RasSceneObject) * expected_objects);
        if (scene->objects == NULL) {
            ras_log_error("Failed to allocate memory for scene objects.");
            mpack_tree_destroy(&tree);
            free(scene->models);
            free(scene);
            return NULL;
        }

        mpack_node_t objects_node = mpack_node_map_cstr(root, "objects");
        scene->num_objects = 0;

        for (size_t i = 0; i < expected_objects; i++) {
            mpack_node_t obj_node = mpack_node_array_at(objects_node, i);
            RasSceneObject* obj = &scene->objects[i];
            if (pack_decode_scene_object(obj_node, obj) != RAS_RESULT_OK) {
                ras_log_error("Failed to decode scene object %zu.", i);
                mpack_tree_destroy(&tree);
                core_free_scene(&scene);
                return NULL;
            }
            scene->num_objects++;
        }
    }

    // clean up and check for errors
    if (mpack_tree_destroy(&tree) != mpack_ok) {
        ras_log_error("An error occurred decoding scene with size %zu.", size);
        core_free_scene(&scene);
        return NULL;
    }
    /* cameras */
    scene->num_cameras = mpack_node_i32(mpack_node_map_cstr(root, "num_cameras"));
    if (scene->num_cameras > 0) {
        scene->cameras = malloc(sizeof(RasCamera) * scene->num_cameras);
        if (scene->cameras == NULL) {
            ras_log_error("Failed to allocate memory for cameras");
            core_free_scene(&scene);
            mpack_tree_destroy(&tree);
            return NULL;
        }

        mpack_node_t cams_node = mpack_node_map_cstr(root, "cameras");
        for (size_t i = 0; i < scene->num_cameras; i++) {
            if (pack_decode_camera(mpack_node_array_at(cams_node, i), &scene->cameras[i]) != RAS_RESULT_OK) {
                ras_log_error("Failed to decode camera %zu", i);
                core_free_scene(&scene);
                mpack_tree_destroy(&tree);
                return NULL;
            }
        }
    }

    return scene;
}
