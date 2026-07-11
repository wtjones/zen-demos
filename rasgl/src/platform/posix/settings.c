
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#include <larse/core/expression.h>
#include <larse/core/merge.h>
#include <larse/core/parse.h>
#include <larse/core/repr.h>
#pragma GCC diagnostic pop
#include "rasgl/core/settings.h"
#include "settings.h"
#include <unistd.h>

RasResult posix_script_map_settings(
    LarScript* script,
    RasPosixSettings* settings)
{
    LarNode* node = lar_get_list_by_symbol(
        script->expressions, RAS_SCRIPT_POSIX_HEAD);

    RAS_CHECK_AND_LOG(node == NULL,
        "Settings expression not found in script");

    node = lar_get_list_by_symbol(
        node, RAS_SCRIPT_POSIX_WINDOW);

    RAS_CHECK_AND_LOG(node == NULL,
        "Settings expression not found in script");

    LarNode* prop = NULL;

    prop = lar_get_property_by_type(node, RAS_SCRIPT_POSIX_X, LAR_NODE_ATOM_INTEGER);
    if (!prop) {
        ras_log_error("Property %s is required", RAS_SCRIPT_POSIX_X);
        return RAS_RESULT_ERROR;
    }
    settings->window.x = prop->atom.val_integer;

    prop = lar_get_property_by_type(node, RAS_SCRIPT_POSIX_Y, LAR_NODE_ATOM_INTEGER);
    if (!prop) {
        ras_log_error("Property %s is required", RAS_SCRIPT_POSIX_Y);
        return RAS_RESULT_ERROR;
    }
    settings->window.y = prop->atom.val_integer;

    prop = lar_get_property_by_type(node, RAS_SCRIPT_POSIX_SCALE_FACTOR, LAR_NODE_ATOM_INTEGER);
    if (!prop) {
        ras_log_error("Property %s is required", RAS_SCRIPT_POSIX_SCALE_FACTOR);
        return RAS_RESULT_ERROR;
    }
    settings->window.scale_factor = prop->atom.val_integer;

    return RAS_RESULT_OK;
}

RasResult posix_load_settings(RasAppSettings* app_settings)
{
    RasResult result = RAS_RESULT_ERROR;

    LarScript* base = NULL;
    LarScript* posix = NULL;
    LarScript* local = NULL;
    LarScript* merged = NULL;

    if (lar_parse_file(RAS_SCRIPT_SETTINGS_PATH, &base) != LAR_PARSE_RESULT_OK) {
        ras_log_error("Unable to load script: %s", RAS_SCRIPT_SETTINGS_PATH);
        return RAS_RESULT_ERROR;
    }

    if (lar_parse_file(RAS_SCRIPT_POSIX_PATH, &posix) != LAR_PARSE_RESULT_OK) {
        ras_log_error("Unable to load script: %s", RAS_SCRIPT_POSIX_PATH);
        goto cleanup;
    }

    if (access(RAS_SCRIPT_POSIX_LOCAL_PATH, F_OK) == 0) {
        if (lar_parse_file(RAS_SCRIPT_POSIX_LOCAL_PATH, &local) != LAR_PARSE_RESULT_OK) {
            ras_log_error("Unable to load script: %s", RAS_SCRIPT_POSIX_LOCAL_PATH);
            goto cleanup;
        }
    }

    merged = lar_merge_script(base, posix);
    if (!merged) {
        ras_log_error("Unable to merge script.");
        goto cleanup;
    }

    lar_free_script(&base);
    lar_free_script(&posix);

#ifdef DEBUG
    char* repr = lar_repr_script(merged);
    ras_log_debug("Merged settings (base, posix):\n%s", repr);
    free(repr);
#endif

    if (local) {
        LarScript* new_merged = lar_merge_script(merged, local);
        if (!new_merged) {
            ras_log_error("Unable to merge script.");
            goto cleanup;
        }
        lar_free_script(&merged);
        merged = new_merged;

#ifdef DEBUG
        repr = lar_repr_script(merged);
        ras_log_debug("Merged settings (base, posix, local):\n%s", repr);
        free(repr);
#endif
    }

    if (hosted_script_map_settings(merged, &app_settings->base) != RAS_RESULT_OK) {
        ras_log_error("Unable to map settings.");
        goto cleanup;
    }

    if (posix_script_map_settings(merged, &app_settings->posix) != RAS_RESULT_OK) {
        ras_log_error("Unable to map posix settings.");
        goto cleanup;
    }

    result = RAS_RESULT_OK;

cleanup:
    lar_free_script(&base);
    lar_free_script(&posix);
    lar_free_script(&local);
    lar_free_script(&merged);
    return result;
}
