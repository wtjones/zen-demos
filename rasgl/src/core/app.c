#include "rasgl/core/app.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/repr.h"

void ras_core_update(InputState* input_state, RenderState render_states[RAS_LAYER_COUNT])
{
    char buffer[255];

    RenderState* scene_state = &render_states[RAS_LAYER_SCENE];
    RenderState* ui_state = &render_states[RAS_LAYER_UI];

    if (input_state->keys[RAS_KEY_P] == RAS_KEY_EVENT_UP) {
        scene_state->projection_mode = scene_state->projection_mode == RAS_PERSPECTIVE_MATRIX
            ? RAS_ORTHO_MATRIX
            : RAS_PERSPECTIVE_MATRIX;
    }
    if (input_state->keys[RAS_KEY_B] == RAS_KEY_EVENT_UP) {
        scene_state->backface_culling_mode = scene_state->backface_culling_mode == RAS_BACKFACE_CULLING_ON
            ? RAS_BACKFACE_CULLING_OFF
            : RAS_BACKFACE_CULLING_ON;

        ras_log_info("backface_culling_mode: %s", scene_state->backface_culling_mode == RAS_BACKFACE_CULLING_ON ? "ON" : "OFF");
    }
    if (input_state->keys[RAS_KEY_C] == RAS_KEY_EVENT_UP) {
        scene_state->clipping_mode = (RasClippingMode)((scene_state->clipping_mode + 1) % RAS_CLIPPING_MODE_COUNT);

        ras_log_info("clipping_mode: %s", repr_clipping_mode(buffer, sizeof(buffer), scene_state->clipping_mode));
    }
    if (input_state->keys[RAS_KEY_N] == RAS_KEY_EVENT_UP) {
        scene_state->normal_mode = (RasNormalMode)((scene_state->normal_mode + 1) % RAS_NORMAL_MODE_COUNT);

        ras_log_info("normal_mode: %s", repr_normal_mode(buffer, sizeof(buffer), scene_state->normal_mode));
    }
    if (input_state->keys[RAS_KEY_O] == RAS_KEY_EVENT_UP) {
        scene_state->polygon_mode = (scene_state->polygon_mode + 1) % 2;

        ras_log_info("polygon_mode: %s", scene_state->polygon_mode == RAS_POLYGON_WIREFRAME ? "wireframe" : "solid");
    }
    if (input_state->keys[RAS_KEY_F3] == RAS_KEY_EVENT_UP) {
        ui_state->layer_visible = !ui_state->layer_visible;
        ras_log_info("UI visible: %s", ui_state->layer_visible ? "true" : "false");
    }
    if (input_state->keys[RAS_KEY_F] == RAS_KEY_EVENT_UP) {
        ras_log_flush();
    }
    ras_log_clear();

    input_state->current_frame = scene_state->current_frame;
}
