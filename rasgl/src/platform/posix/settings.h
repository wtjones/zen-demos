#ifndef POSIX_SETTINGS_H
#define POSIX_SETTINGS_H

#include "rasgl/core/graphics.h"
#include "rasgl/core/settings.h"
#include "rasgl/hosted/settings_load.h"

#define RAS_SCRIPT_POSIX_PATH "config/posix.lsp"
#define RAS_SCRIPT_POSIX_LOCAL_PATH "config/posix-local.lsp"

typedef struct RasPosixSettings {
    uint32_t scale_factor;
} RasPosixSettings;

typedef struct RasAppSettings {
    RasSettings base;
    RasPosixSettings posix;

} RasAppSettings;

RasResult posix_load_settings(RasAppSettings* app_settings);
#endif
