#include "rasgl/core/scene.h"
#include <string.h>

RasResult core_script_map_name(LarNode* scene_exp, RasScene* scene)
{

    LarNode* scene_name = lar_get_property_by_type(
        scene_exp, SCRIPT_SYMBOL_NAME, LAR_NODE_ATOM_STRING);

    if (scene_name == NULL) {
        ras_log_error("Scene %s is required", SCRIPT_SYMBOL_NAME);
        return RAS_RESULT_ERROR;
    }

    strcpy(scene->name, scene_name->atom.val_string);
    return RAS_RESULT_OK;
}

RasResult core_script_map_models(LarNode* scene_exp, RasScene* scene)
{
    LarNode* models_list = lar_get_list_by_symbol(
        scene_exp, SCRIPT_SYMBOL_MODELS);

    if (models_list == NULL) {
        ras_log_error("Models list not found in script");
        return RAS_RESULT_ERROR;
    }

    size_t num_models = models_list->list.length - 1; // exclude the symbol

    RasSceneModel* models = (RasSceneModel*)malloc(
        sizeof(RasSceneModel) * num_models);

    if (models == NULL) {
        ras_log_error("Failed to allocate memory for models");
        return RAS_RESULT_ERROR;
    }

    for (size_t i = 0; i < num_models; i++) {
        // skip the symbol
        LarNode* model_exp = lar_get_list_node_by_index(models_list, i + 1);

        RasSceneModel* model = &models[i];

        LarNode* model_name = lar_get_property_by_type(
            model_exp, SCRIPT_SYMBOL_NAME, LAR_NODE_ATOM_STRING);

        if (model_name == NULL) {
            ras_log_error("Symbol '%s' is required", SCRIPT_SYMBOL_NAME);
            free(models);
            return RAS_RESULT_ERROR;
        }

        strcpy(model->name, model_name->atom.val_string);

        LarNode* model_path = lar_get_property_by_type(
            model_exp, SCRIPT_SYMBOL_PATH, LAR_NODE_ATOM_STRING);

        if (model_path == NULL) {
            ras_log_error("Property %s is required", SCRIPT_SYMBOL_PATH);
            free(models);
            return RAS_RESULT_ERROR;
        }

        strcpy(model->path, model_path->atom.val_string);
    }

    scene->models = models;
    scene->num_models = num_models;

    return RAS_RESULT_OK;
}

RasResult core_script_map_scene(LarScript* script, RasScene** scene)
{
    LarNode* scene_exp = lar_get_list_by_symbol(
        script->expressions, SCRIPT_SYMBOL_SCENE);

    if (scene_exp == NULL) {
        ras_log_error("Scene expression not found in script");

        return RAS_RESULT_ERROR;
    }

    RasScene* new_scene = (RasScene*)malloc(sizeof(RasScene));

    if (new_scene == NULL) {
        ras_log_error("Failed to allocate memory for scene");
        return RAS_RESULT_ERROR;
    }

    RasResult result = core_script_map_name(scene_exp, new_scene);
    if (result != RAS_RESULT_OK) {
        ras_log_error("Failed to map scene name");
        free(new_scene);
        return RAS_RESULT_ERROR;
    }

    result = core_script_map_models(scene_exp, new_scene);

    if (result != RAS_RESULT_OK) {
        ras_log_error("Failed to map models");
        free(new_scene);
        return RAS_RESULT_ERROR;
    }

    *scene = new_scene;

    return RAS_RESULT_OK;
}

RasResult core_load_scene(const char* path, RasScene** scene)
{
    LarScript* script;
    LarParseResult result = lar_parse_file(path, &script);

    if (result != LAR_PARSE_RESULT_OK) {
        ras_log_error("Failed to parse scene file: %s", path);
        return RAS_RESULT_ERROR;
    }
    char* repr = lar_repr_script(script);
    ras_log_info("Script: %s", repr);
    free(repr);

    result = core_script_map_scene(script, scene);
    if (result != RAS_RESULT_OK) {
        ras_log_error("Failed to map script to scene");
        lar_free_script(&script);

        return RAS_RESULT_ERROR;
    }
    lar_free_script(&script);

    return RAS_RESULT_OK;
}

void core_free_scene(RasScene** scene)
{
    RasScene* s = *scene;
    free(s->models);
    free(s);
    *scene = NULL;
}
