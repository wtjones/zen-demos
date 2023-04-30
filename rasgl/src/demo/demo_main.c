#include "rasgl/core/app.h"
#include "rasgl/core/graphics.h"
#include <stdio.h>

Vector2f demo_pos;
ScreenSettings* settings;

void ras_app_init(int argc, const char** argv, ScreenSettings* init_settings)
{
    printf("ras_app_init()... screen_width.x: %d\n", init_settings->screen_width);
    settings = init_settings;
    demo_pos.x = settings->screen_width / 2;
    demo_pos.y = settings->screen_height / 2;
}

void ras_app_update()
{
    demo_pos.x++;
    if (demo_pos.x >= settings->screen_width) {
        demo_pos.x = 0;
    }
}

void ras_app_render(RenderState* render_state)
{
    render_state->points[0].x = demo_pos.x;
    render_state->points[0].y = demo_pos.y;
    render_state->num_points = 1;
    render_state->commands[0].point_indices[0] = 0;
    render_state->commands[0].num_points = 1;
    render_state->num_commands = 1;
}
