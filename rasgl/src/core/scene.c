#include "rasgl/core/scene.h"
#include <string.h>

RasResult core_script_map_scene(LarScript* script, RasScene** scene)
{

    if (script->expressions->list.length == 0) {
        ras_log_error("No expressions in script");
        return RAS_RESULT_ERROR;
    }

    LarNode* scene_exp = &script->expressions->list.nodes[0];

    if (scene_exp->list.length < 3) {
        ras_log_error("Scene expression must have at least 3 elements");
        return RAS_RESULT_ERROR;
    }

    LarNode* symbol = &scene_exp->list.nodes[0];
    if (symbol->node_type != LAR_NODE_ATOM_SYMBOL
        || strcmp(symbol->atom.val_symbol, SCRIPT_SYMBOL_SCENE) != 0) {
        ras_log_error("Symbol 'scene' expected in expression");
        return RAS_RESULT_ERROR;
    }

    LarNode* scene_name_symbol = &scene_exp->list.nodes[1];
    LarNode* scene_name_string = &scene_exp->list.nodes[2];

    if (scene_name_symbol->node_type != LAR_NODE_ATOM_SYMBOL
        || strcmp(scene_name_symbol->atom.val_symbol, ":name") != 0) {
        ras_log_error("Scene :name is required");
        return RAS_RESULT_ERROR;
    }

    if (scene_name_string->node_type != LAR_NODE_ATOM_STRING) {
        ras_log_error("Scene :name must be a string");
        return RAS_RESULT_ERROR;
    }

    RasScene* new_scene = (RasScene*)malloc(sizeof(RasScene));
    strcpy(new_scene->name, scene_name_string->atom.val_string);

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
