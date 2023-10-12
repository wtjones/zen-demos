#include "rasgl/core/app.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/fixed_maths.h"
#include "rasgl/core/frustum.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/input.h"
#include "rasgl/core/repr.h"
#include <stdio.h>
#include <stdlib.h>

ScreenSettings* settings;

Point3f vertex_buffer[] = {
    { float_to_fixed_16_16(-1.0), float_to_fixed_16_16(-1.0) },
    { float_to_fixed_16_16(1.0), float_to_fixed_16_16(-1.0) },
    { float_to_fixed_16_16(-1.0), float_to_fixed_16_16(1.0) },
    { float_to_fixed_16_16(1.0), float_to_fixed_16_16(1.0) }
};

uint32_t element_buffer[] = { 0, 1, 2, 3 };

void ras_app_init(int argc, const char** argv, ScreenSettings* init_settings)
{
    ras_log_info("ras_app_init()... argc: %d argv: %s\n", argc, argv[0]);
    ras_log_info("ras_app_init()... screen_width.x: %d\n", init_settings->screen_width);
    settings = init_settings;
}

void ras_app_update()
{
}

void ras_app_render(RenderState* render_state)
{
    render_state->num_points = 0;
    render_state->num_commands = 0;
}
