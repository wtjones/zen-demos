#include "rasgl/core/scene.h"
#include <string.h>

RasResult core_script_map_scene(LarScript* script, RasScene** scene)
{
    LarNode* scene_exp = lar_get_list_by_symbol(
        script->expressions, SCRIPT_SYMBOL_SCENE);

    if (scene_exp == NULL) {
        ras_log_error("Scene expression not found in script");

        return RAS_RESULT_ERROR;
    }

    LarNode* scene_name = lar_get_property_by_type(
        scene_exp, SCRIPT_SYMBOL_NAME, LAR_NODE_ATOM_STRING);

    if (scene_name == NULL) {
        ras_log_error("Scene %s is required", SCRIPT_SYMBOL_NAME);
        return RAS_RESULT_ERROR;
    }

    RasScene* new_scene = (RasScene*)malloc(sizeof(RasScene));
    strcpy(new_scene->name, scene_name->atom.val_string);

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
