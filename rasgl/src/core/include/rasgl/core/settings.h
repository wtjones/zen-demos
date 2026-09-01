#ifndef CORE_SETTINGS_H
#define CORE_SETTINGS_H

#include "console.h"
#include "graphics.h"

typedef struct RasSettings {
    ScreenSettings screen;
    RasConsoleSettings console;
} RasSettings;

RasResult core_load_settings(const char* path, RasSettings* settings);

#endif
