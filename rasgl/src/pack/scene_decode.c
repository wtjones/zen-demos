#include "rasgl/core/scene.h"
#include "rasgl/pack/pack.h"
#include <stdlib.h>
#include <string.h>

RasScene* pack_decode_scene(const char* data, size_t size)
{
    // Use 'commit-on-success' pattern to avoid having to free partially
    // decoded data on failure.
    // See also libglt2d's gltf parsing for a similar approach.
    mpack_tree_t tree;
    RasScene tmp = { 0 };
    RasScene* scene = NULL;

    mpack_tree_init(&tree, data, size);
    mpack_tree_parse(&tree);

    mpack_node_t root = mpack_tree_root(&tree);

    mpack_node_copy_cstr(mpack_node_map_cstr(root, "name"), tmp.name, MAX_SCENE_NAME);
    tmp.num_models = mpack_node_i32(mpack_node_map_cstr(root, "num_models"));

    if (tmp.num_models > 0) {
        tmp.models = malloc(sizeof(RasSceneModel) * tmp.num_models);
        if (tmp.models == NULL) {
            ras_log_error("Failed to allocate memory for scene models.");
            goto fail;
        }

        mpack_node_t models_node = mpack_node_map_cstr(root, "models");

        for (size_t i = 0; i < tmp.num_models; i++) {
            mpack_node_t model_node = mpack_node_array_at(models_node, i);
            if (pack_decode_scene_model(model_node, &tmp.models[i]) != RAS_RESULT_OK) {
                ras_log_error("Failed to decode scene model %zu.", i);
                goto fail;
            }
        }
    }

    size_t expected_objects = mpack_node_i32(mpack_node_map_cstr(root, "num_objects"));
    if (expected_objects > 0) {
        tmp.objects = malloc(sizeof(RasSceneObject) * expected_objects);
        if (tmp.objects == NULL) {
            ras_log_error("Failed to allocate memory for scene objects.");
            goto fail;
        }

        mpack_node_t objects_node = mpack_node_map_cstr(root, "objects");
        tmp.num_objects = 0;

        for (size_t i = 0; i < expected_objects; i++) {
            mpack_node_t obj_node = mpack_node_array_at(objects_node, i);
            RasSceneObject* obj = &tmp.objects[i];
            if (pack_decode_scene_object(obj_node, obj) != RAS_RESULT_OK) {
                ras_log_error("Failed to decode scene object %zu.", i);
                goto fail;
            }
            tmp.num_objects++;
        }
    }

    /* tombmaps */
    tmp.num_tombmaps = mpack_node_i32(mpack_node_map_cstr(root, "num_tombmaps"));
    if (tmp.num_tombmaps > 0) {
        tmp.tombmaps = malloc(sizeof(RasSceneTombMap) * tmp.num_tombmaps);
        if (tmp.tombmaps == NULL) {
            ras_log_error("Failed to allocate memory for tombmaps.");
            goto fail;
        }
        mpack_node_t tombs_node = mpack_node_map_cstr(root, "tombmaps");
        for (size_t i = 0; i < tmp.num_tombmaps; i++) {
            if (pack_decode_tombmap(mpack_node_array_at(tombs_node, i), &tmp.tombmaps[i]) != RAS_RESULT_OK) {
                ras_log_error("Failed to decode tombmap %zu.", i);
                goto fail;
            }
        }
    }

    /* cameras */
    tmp.num_cameras = mpack_node_i32(mpack_node_map_cstr(root, "num_cameras"));
    if (tmp.num_cameras > 0) {
        tmp.cameras = malloc(sizeof(RasCamera) * tmp.num_cameras);
        if (tmp.cameras == NULL) {
            ras_log_error("Failed to allocate memory for cameras");
            goto fail;
        }

        mpack_node_t cams_node = mpack_node_map_cstr(root, "cameras");
        for (size_t i = 0; i < tmp.num_cameras; i++) {
            if (pack_decode_camera(mpack_node_array_at(cams_node, i), &tmp.cameras[i]) != RAS_RESULT_OK) {
                ras_log_error("Failed to decode camera %zu", i);
                goto fail;
            }
        }
    }

    /* commit */
    scene = malloc(sizeof(RasScene));
    if (scene == NULL) {
        ras_log_error("Failed to allocate memory for scene.");
        goto fail;
    }
    *scene = tmp;

    if (mpack_tree_destroy(&tree) != mpack_ok) {
        ras_log_error("An error occurred decoding scene with size %zu.", size);
        core_free_scene(&scene);
        return NULL;
    }

    return scene;

fail:
    if (tmp.models) {
        for (size_t i = 0; i < tmp.num_models; i++) {
            core_pipeline_element_free(&tmp.models[i].element);
        }
        free(tmp.models);
        tmp.models = NULL;
        tmp.num_models = 0;
    }

    if (tmp.objects) {
        for (size_t i = 0; i < tmp.num_objects; i++) {
            if (tmp.objects[i].animations) {
                free(tmp.objects[i].animations);
                tmp.objects[i].animations = NULL;
                tmp.objects[i].num_animations = 0;
            }
        }
        free(tmp.objects);
        tmp.objects = NULL;
        tmp.num_objects = 0;
    }

    if (tmp.cameras) {
        free(tmp.cameras);
        tmp.cameras = NULL;
        tmp.num_cameras = 0;
    }

    if (tmp.gridmaps) {
        for (size_t i = 0; i < tmp.num_gridmaps; i++) {
            core_pipeline_element_free(&tmp.gridmaps[i].element);
        }
        free(tmp.gridmaps);
        tmp.gridmaps = NULL;
        tmp.num_gridmaps = 0;
    }

    if (tmp.tombmaps) {
        core_free_scene_tombmaps(tmp.tombmaps, tmp.num_tombmaps);
        tmp.tombmaps = NULL;
        tmp.num_tombmaps = 0;
    }

    mpack_tree_destroy(&tree);
    return NULL;
}
