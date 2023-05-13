#include "rasgl/core/app.h"
#include "rasgl/core/fixed_maths.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/input.h"
#include <stdio.h>

#define CELL_UNITS INT_32_TO_FIXED_16_16(10);

Point3f viewer_pos;
ScreenSettings* settings;

int map[5][5] = {
    { 1, 1, 1, 1, 1 },
    { 1, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 1 },
    { 1, 1, 1, 1, 1 }
};

void ras_app_init(int argc, const char** argv, ScreenSettings* init_settings)
{
    printf("ras_app_init()... screen_width.x: %d\n", init_settings->screen_width);
    settings = init_settings;

    viewer_pos.x = INT_32_TO_FIXED_16_16(25);
    viewer_pos.y = INT_32_TO_FIXED_16_16(25);
    viewer_pos.z = INT_32_TO_FIXED_16_16(1);
}

void ras_app_update(InputState* input_state)
{
    if (input_state->keys[RAS_KEY_W] == 1) {
        viewer_pos.y += INT_32_TO_FIXED_16_16(1);
    }
    if (input_state->keys[RAS_KEY_A] == 1) {
        viewer_pos.x -= INT_32_TO_FIXED_16_16(1);
    }
    if (input_state->keys[RAS_KEY_S] == 1) {
        viewer_pos.y -= INT_32_TO_FIXED_16_16(1);
    }
    if (input_state->keys[RAS_KEY_D] == 1) {
        viewer_pos.x += INT_32_TO_FIXED_16_16(1);
    }
}

void render_map(RenderState* render_state)
{
    Point2f dest;
    Point2i* screen_pos;
    Point2f source;
    size_t* num_points = &render_state->num_points;
    size_t* num_commands = &render_state->num_commands;

    for (int r = 0; r < 5; r++) {
        for (int c = 0; c < 5; c++) {

            screen_pos = &render_state->points[*num_points];
            // world space of map node
            source.x = c * CELL_UNITS;
            source.y = r * CELL_UNITS;

            dest.x = 0;
            dest.y = 0;
            xform_to_screen(
                settings->screen_width,
                settings->screen_height,
                &viewer_pos, &source, &dest);

            screen_pos->x = FIXED_16_16_TO_INT_32(dest.x);
            // TODO: flip y for world -> screen
            screen_pos->y = FIXED_16_16_TO_INT_32(dest.y);

            render_state->commands[*num_commands].num_points = 1;
            render_state->commands[*num_commands].point_indices[0] = *num_points;

            (*num_points)++;
            (*num_commands)++;
        }
    }
}

void ras_app_render(RenderState* render_state)
{
    render_state->num_points = 0;
    render_state->num_commands = 0;
    render_map(render_state);
}
