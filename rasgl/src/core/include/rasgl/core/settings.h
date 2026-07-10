#ifndef CORE_SETTINGS_H
#define CORE_SETTINGS_H

#include "graphics.h"

typedef struct RasSettings {
    ScreenSettings screen;
} RasSettings;

RasResult core_load_settings(const char* path, RasSettings* settings);

#endif
