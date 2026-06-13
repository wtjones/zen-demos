#ifndef POSIX_SETTINGS_H
#define POSIX_SETTINGS_H

#include "rasgl/core/debug.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#include <larse/core/expression.h>
#include <larse/core/parse.h>
#include <larse/core/repr.h>
#pragma GCC diagnostic pop

#include "rasgl/core/graphics.h"

#define SCRIPT_SYMBOL_SCREEN "screen"
#define SCRIPT_SYMBOL_SCREEN_WIDTH ":width"
#define SCRIPT_SYMBOL_SCREEN_HEIGHT ":height"
#define SCRIPT_SYMBOL_SCREEN_SCALE ":scale"

typedef struct RasPosixSettings {
    ScreenSettings screen_settings;
} RasPosixSettings;

RasResult posix_script_load_settings(
    const char* path, RasPosixSettings* settings);

#endif
