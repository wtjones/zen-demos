#ifndef APP_H
#define APP_H

#include "debug.h"
#include "graphics.h"
#include "input.h"

RasResult ras_app_init(int argc, const char** argv, ScreenSettings* video_settings);
RasResult ras_app_renderstates_init(RenderState states[]);
void ras_app_update(InputState*);
void ras_app_render(__attribute__((unused)) RenderState states[RAS_LAYER_COUNT]);
void app_main();

/**
 * Handle common key bindings
 */
void ras_core_update(InputState* input_state, RenderState states[RAS_LAYER_COUNT]);

#endif
