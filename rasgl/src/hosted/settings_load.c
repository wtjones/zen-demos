#include "rasgl/hosted/settings_load.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/settings.h"

RasResult hosted_script_map_settings(
    LarScript* script,
    RasSettings* settings)
{

    LarNode* node = lar_get_list_by_symbol(
        script->expressions, RAS_SCRIPT_SETTINGS_SCREEN);

    RAS_CHECK_AND_LOG(node == NULL,
        "Settings expression not found in script");

    LarNode* prop = NULL;

    prop = lar_get_property_by_type(node, RAS_SCRIPT_SETTINGS_SCREEN_WIDTH, LAR_NODE_ATOM_INTEGER);
    if (!prop) {
        ras_log_error("Property %s is required", RAS_SCRIPT_SETTINGS_SCREEN_WIDTH);
        return RAS_RESULT_ERROR;
    }
    settings->screen.screen_width = prop->atom.val_integer;

    prop = lar_get_property_by_type(node, RAS_SCRIPT_SETTINGS_SCREEN_HEIGHT, LAR_NODE_ATOM_INTEGER);
    if (!prop) {
        ras_log_error("Property %s is required", RAS_SCRIPT_SETTINGS_SCREEN_HEIGHT);
        return RAS_RESULT_ERROR;
    }
    settings->screen.screen_height = prop->atom.val_integer;

    return RAS_RESULT_OK;
}

/**
 * @brief Load base settings from a script file path.
 *
 */
RasResult script_load_settings(const char* path, RasSettings* settings)
{
    LarScript* script;
    LarParseResult result = lar_parse_file(path, &script);

    RAS_CHECK_AND_LOG(result != LAR_PARSE_RESULT_OK,
        "Failed to parse file: %s", path);

    char* repr = lar_repr_script(script);
    ras_log_debug("Script: %s", repr);
    free(repr);

    result = hosted_script_map_settings(script, settings);

    if (result != RAS_RESULT_OK) {
        ras_log_error("Failed to map script to settings");
        lar_free_script(&script);

        return RAS_RESULT_ERROR;
    }
    lar_free_script(&script);

    return RAS_RESULT_OK;
}

/**
 * @brief Implementation of core_load_settings() from core.
 *
 * @param path
 * @param settings
 * @return RasResult
 */
RasResult core_load_settings(const char* path, RasSettings* settings)
{
    if (!settings) {
        ras_log_error("Invalid pointer.");
        return RAS_RESULT_ERROR;
    }

    size_t plen = strlen(path);
    if (plen >= 3 && strcmp(path + plen - 3, ".mp") == 0) {
        ras_log_error("Not implemented");
        return RAS_RESULT_ERROR;
    }
    return script_load_settings(path, settings);
}
