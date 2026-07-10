
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#include <larse/core/expression.h>
#include <larse/core/parse.h>
#include <larse/core/repr.h>
#pragma GCC diagnostic pop
#include "rasgl/core/settings.h"
#include "settings.h"

RasResult posix_load_settings(RasAppSettings* app_settings)
{
    LarScript* base = NULL;

    if (lar_parse_file(RAS_SCRIPT_SETTINGS_PATH, &base) != LAR_PARSE_RESULT_OK) {
        ras_log_error("Unable to load script");
        return RAS_RESULT_ERROR;
    }

    if (hosted_script_map_settings(base, &app_settings->base) != RAS_RESULT_OK) {
        ras_log_error("Unable to map settings");
        lar_free_script(&base);
        return RAS_RESULT_ERROR;
    }
#ifdef DEBUG
    char* repr = lar_repr_script(base);
    ras_log_debug("App settings:\n%s", repr);
    free(repr);
#endif
    lar_free_script(&base);

    return RAS_RESULT_OK;
}
