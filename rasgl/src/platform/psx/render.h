#ifndef PSX_RENDER_H
#define PSX_RENDER_H

#include "rasgl/core/graphics.h"
#include <stdint.h>

#ifndef RAS_MAX_FRAMES
#    define RAS_MAX_FRAMES -1
#endif

RasResult render_renderstates_init(RenderState* states);

void render_mesh_solid(RenderState* state);
void render_mesh_wireframe(RenderState* state);
void render_mesh_bitmap(RenderState* state);

void render_clear(ScreenSettings* plat_settings);
void render_state(RenderState* state);
void render_flip();

#endif
