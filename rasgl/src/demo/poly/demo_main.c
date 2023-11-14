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
    render_state->max_frames = 1; // render single frame

    // 2 ------ 3
    // |  \     |
    // |     \  |
    // 0 -------1
    RasVertex vertex_buffer[] = {
        { .position = { float_to_fixed_16_16(-0.5), float_to_fixed_16_16(-0.5), float_to_fixed_16_16(1.0) } },
        { .position = { float_to_fixed_16_16(0.5), float_to_fixed_16_16(-0.5), float_to_fixed_16_16(1.0) } },
        { .position = { float_to_fixed_16_16(-0.5), float_to_fixed_16_16(0.5), float_to_fixed_16_16(1.0) } },
        { .position = { float_to_fixed_16_16(0.5), float_to_fixed_16_16(0.5), float_to_fixed_16_16(1.0) } }
    };

    uint32_t num_verts = 4;
    uint32_t element_indexes[] = { 0, 1, 2, 2, 1, 3 };
    uint32_t num_indexes = 6;
    int32_t model_world_matrix[4][4];
    int32_t world_view_matrix[4][4];
    int32_t projection_matrix[4][4];

    mat_ortho_init(
        projection_matrix,
        -INT_32_TO_FIXED_16_16(1),
        INT_32_TO_FIXED_16_16(1),
        -INT_32_TO_FIXED_16_16(1),
        INT_32_TO_FIXED_16_16(1),
        -INT_32_TO_FIXED_16_16(1),
        INT_32_TO_FIXED_16_16(1));

    mat_set_identity_4x4(model_world_matrix);
    mat_set_identity_4x4(world_view_matrix);

    core_draw_elements(
        render_state,
        vertex_buffer,
        num_verts,
        element_indexes,
        num_indexes,
        model_world_matrix,
        world_view_matrix,
        projection_matrix);
}
