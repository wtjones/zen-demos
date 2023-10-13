#include "rasgl/core/app.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/fixed_maths.h"
#include "rasgl/core/frustum.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/input.h"
#include "rasgl/core/matrix_projection.h"
#include "rasgl/core/repr.h"
#include <stdio.h>
#include <stdlib.h>

ScreenSettings* settings;
uint32_t element_buffer[] = { 0, 1, 2, 3 };

void ras_app_init(int argc, const char** argv, ScreenSettings* init_settings)
{
    ras_log_info("ras_app_init()... argc: %d argv: %s\n", argc, argv[0]);
    ras_log_info("ras_app_init()... screen_width.x: %d\n", init_settings->screen_width);
    settings = init_settings;
}

void ras_app_update(__attribute__((unused)) InputState* input_state)
{
}

void ras_app_render(RenderState* render_state)
{
    Point3f vertex_buffer[] = {
        { float_to_fixed_16_16(-0.5), float_to_fixed_16_16(-0.5), float_to_fixed_16_16(1.0) },
        { float_to_fixed_16_16(0.5), float_to_fixed_16_16(-0.5), float_to_fixed_16_16(1.0) },
        { float_to_fixed_16_16(-0.5), float_to_fixed_16_16(0.5), float_to_fixed_16_16(1.0) },
        { float_to_fixed_16_16(0.5), float_to_fixed_16_16(0.5), float_to_fixed_16_16(1.0) }
    };

    uint32_t* num_points = &render_state->num_points;
    uint32_t* num_commands = &render_state->num_commands;

    int32_t combined_matrix[4][4];
    int32_t view_matrix[4][4];
    int32_t projection_matrix[4][4];
    int32_t projected_point[4];

    mat_ortho_init(
        projection_matrix,
        -INT_32_TO_FIXED_16_16(1),
        INT_32_TO_FIXED_16_16(1),
        -INT_32_TO_FIXED_16_16(1),
        INT_32_TO_FIXED_16_16(1),
        -INT_32_TO_FIXED_16_16(1),
        INT_32_TO_FIXED_16_16(1));

    mat_set_identity_4x4(view_matrix);

    mat_mul_4x4_4x4(projection_matrix, view_matrix, combined_matrix);

    render_state->num_points = 0;
    render_state->num_commands = 0;
    for (size_t i = 0; i < 4; i++) {

        int32_t v[4] = {
            vertex_buffer[i].x,
            vertex_buffer[i].y,
            vertex_buffer[i].z,
            float_to_fixed_16_16(1.0)
        };

        mat_mul_project(combined_matrix, v, projected_point);

        RenderCommand* command = &render_state->commands[*num_commands];
        Point2i* screen_point = &render_state->points[*num_points];

        projected_to_screen_point(settings->screen_width, settings->screen_height, projected_point, screen_point);

        command->num_points = 1;
        command->point_indices[0] = *num_points;
        (*num_commands)++;
        (*num_points)++;
    }
}
