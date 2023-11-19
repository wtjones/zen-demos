#include "rasgl/core/app.h"

void ras_core_update(InputState* input_state, RenderState* render_state)
{
    // FIXME: Use key up for toggles
    if (input_state->keys[RAS_KEY_P] == 1) {
        render_state->projection_mode = render_state->projection_mode == RAS_PERSPECTIVE_MATRIX
            ? RAS_ORTHO_MATRIX
            : RAS_PERSPECTIVE_MATRIX;
    }
}
