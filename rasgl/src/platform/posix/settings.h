#ifndef POSIX_SETTINGS_H
#define POSIX_SETTINGS_H

#include "rasgl/core/graphics.h"
#include "rasgl/core/settings.h"
#include "rasgl/hosted/settings_load.h"

#define RAS_SCRIPT_POSIX_PATH "config/posix.lsp"
#define RAS_SCRIPT_POSIX_LOCAL_PATH "config/posix-local.lsp"
#define RAS_SCRIPT_POSIX_HEAD "posix"
#define RAS_SCRIPT_POSIX_WINDOW "window"
#define RAS_SCRIPT_POSIX_X ":x"
#define RAS_SCRIPT_POSIX_Y ":y"
#define RAS_SCRIPT_POSIX_SCALE_FACTOR ":scale_factor"

typedef struct RasPosixSettings {
    struct RasPosixWindow {
        uint32_t x;
        uint32_t y;
        uint32_t scale_factor;
    } window;
} RasPosixSettings;

typedef struct RasAppSettings {
    RasSettings base;
    RasPosixSettings posix;

} RasAppSettings;

RasResult posix_load_settings(RasAppSettings* app_settings);
#endif
