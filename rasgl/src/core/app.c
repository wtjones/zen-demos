#include "rasgl/core/app.h"
#include "rasgl/core/debug.h"

void ras_core_update(InputState* input_state, RenderState* render_state)
{
    char buffer[255];

    // FIXME: Use key up for toggles
    if (input_state->keys[RAS_KEY_P] == RAS_KEY_EVENT_UP) {
        render_state->projection_mode = render_state->projection_mode == RAS_PERSPECTIVE_MATRIX
            ? RAS_ORTHO_MATRIX
            : RAS_PERSPECTIVE_MATRIX;
    }
    if (input_state->keys[RAS_KEY_B] == RAS_KEY_EVENT_UP) {
        render_state->backface_culling_mode = render_state->backface_culling_mode == RAS_BACKFACE_CULLING_ON
            ? RAS_BACKFACE_CULLING_OFF
            : RAS_BACKFACE_CULLING_ON;

        ras_log_info("backface_culling_mode: %s", render_state->backface_culling_mode == RAS_BACKFACE_CULLING_ON ? "ON" : "OFF");
    }
    if (input_state->keys[RAS_KEY_O] == RAS_KEY_EVENT_UP) {
        render_state->polygon_mode = (render_state->polygon_mode + 1) % 2;

        ras_log_info("polygon_mode: %s", render_state->polygon_mode == RAS_POLYGON_WIREFRAME ? "wireframe" : "solid");
    }
    if (input_state->keys[RAS_KEY_F] == RAS_KEY_EVENT_UP) {
        ras_log_flush();
    }
    ras_log_clear();

    input_state->current_frame = render_state->current_frame;
}
