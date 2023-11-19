#ifndef APP_H
#define APP_H

#include "graphics.h"
#include "input.h"

void ras_app_init(int argc, const char** argv, ScreenSettings* video_settings);
void ras_app_update(InputState*);
void ras_app_render(RenderState*);
void app_main();

/**
 * Handle common key bindings
 */
void ras_core_update(InputState* input_state, RenderState* state);

#endif
