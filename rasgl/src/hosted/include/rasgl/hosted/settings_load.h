#ifndef HOSTED_SETTINGS_LOAD_H
#define HOSTED_SETTINGS_LOAD_H

#include "rasgl/core/graphics.h"
#include "rasgl/core/settings.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#include <larse/core/expression.h>
#include <larse/core/parse.h>
#include <larse/core/repr.h>
#pragma GCC diagnostic pop

#define RAS_SCRIPT_SETTINGS_PATH "config/settings.lsp"
#define RAS_SCRIPT_SETTINGS_SCREEN "screen"
#define RAS_SCRIPT_SETTINGS_SCREEN_WIDTH ":width"
#define RAS_SCRIPT_SETTINGS_SCREEN_HEIGHT ":height"

RasResult hosted_script_map_settings(
    LarScript* script,
    RasSettings* settings);

#endif
