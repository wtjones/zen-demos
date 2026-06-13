
#include "settings.h"

RasResult posix_script_map_settings(
    LarScript* script,
    RasPosixSettings* settings)
{
    LarNode* screen_exp = lar_get_list_by_symbol(
        script->expressions, SCRIPT_SYMBOL_SCREEN);

    RAS_CHECK_AND_LOG(screen_exp == NULL,
        "Screen expression not found in script");
    LarNode* node;
    node = lar_get_property_by_type(
        screen_exp, SCRIPT_SYMBOL_SCREEN_WIDTH, LAR_NODE_ATOM_INTEGER);

    if (node) {
        settings->screen_settings.screen_width = node->atom.val_integer;
    }
    node = lar_get_property_by_type(
        screen_exp, SCRIPT_SYMBOL_SCREEN_HEIGHT, LAR_NODE_ATOM_INTEGER);

    if (node) {
        settings->screen_settings.screen_height = node->atom.val_integer;
    }

    node = lar_get_property_by_type(
        screen_exp, SCRIPT_SYMBOL_SCREEN_SCALE, LAR_NODE_ATOM_INTEGER);

    if (node) {
        settings->screen_settings.scale = node->atom.val_integer;
    }

    return RAS_RESULT_OK;
}

RasResult posix_script_load_settings(
    const char* path, RasPosixSettings* settings)
{
    LarScript* script;
    LarParseResult result = lar_parse_file(path, &script);

    RAS_CHECK_AND_LOG(result != LAR_PARSE_RESULT_OK,
        "Failed to parse file: %s", path);

    char* repr = lar_repr_script(script);
    ras_log_debug("Settings: %s", repr);
    free(repr);

    result = posix_script_map_settings(script, settings);

    if (result != RAS_RESULT_OK) {
        ras_log_error("Failed to map script to settings");
        lar_free_script(&script);

        return RAS_RESULT_ERROR;
    }
    lar_free_script(&script);

    return RAS_RESULT_OK;
}
