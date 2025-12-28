#include "rasgl/core/app.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/repr.h"

void ras_core_update(InputState* input_state, RenderState render_states[RAS_LAYER_COUNT])
{
    char buffer[255];

    RenderState* scene_state = &render_states[RAS_LAYER_SCENE];
    RenderState* ui_state = &render_states[RAS_LAYER_UI];
    RenderState* console_state = &render_states[RAS_LAYER_CONSOLE];

    if (input_state->keys[RAS_KEY_P] == RAS_KEY_EVENT_UP
        && input_state->mods & RAS_KMOD_CTRL) {
        scene_state->projection_mode = scene_state->projection_mode == RAS_PERSPECTIVE_MATRIX
            ? RAS_ORTHO_MATRIX
            : RAS_PERSPECTIVE_MATRIX;
    }
    if (input_state->keys[RAS_KEY_B] == RAS_KEY_EVENT_UP
        && input_state->mods & RAS_KMOD_CTRL) {
        scene_state->backface_culling_mode = scene_state->backface_culling_mode == RAS_BACKFACE_CULLING_ON
            ? RAS_BACKFACE_CULLING_OFF
            : RAS_BACKFACE_CULLING_ON;

        ras_log_info("backface_culling_mode: %s", scene_state->backface_culling_mode == RAS_BACKFACE_CULLING_ON ? "ON" : "OFF");
    }
    if (input_state->keys[RAS_KEY_Z] == RAS_KEY_EVENT_UP
        && input_state->mods & RAS_KMOD_CTRL) {
        scene_state->z_divide_mode = (RasZDivideMode)((scene_state->z_divide_mode + 1) % RAS_Z_DIVIDE_MODE_COUNT);

        ras_log_info("z_divide_mode: %s", scene_state->z_divide_mode == RAS_Z_DIVIDE_MODE_LUT ? "LUT" : "RT");
    }
    if (input_state->keys[RAS_KEY_C] == RAS_KEY_EVENT_UP
        && input_state->mods & RAS_KMOD_CTRL
        && !(input_state->mods & RAS_KMOD_SHIFT)) {
        scene_state->clipping_mode = (RasClippingMode)((scene_state->clipping_mode + 1) % RAS_CLIPPING_MODE_COUNT);

        ras_log_info("clipping_mode: %s", repr_clipping_mode(buffer, sizeof(buffer), scene_state->clipping_mode));
    }
    if (input_state->keys[RAS_KEY_C] == RAS_KEY_EVENT_UP
        && input_state->mods & RAS_KMOD_CTRL
        && input_state->mods & RAS_KMOD_SHIFT) {
        scene_state->clip_side_mode = (RasClipSideMode)((scene_state->clip_side_mode + 1) % RAS_CLIP_SIDE_MODE_COUNT);

        ras_log_info("clip_side_mode: %s", repr_clip_side_mode(buffer, sizeof(buffer), scene_state->clip_side_mode));
    }
    if (input_state->keys[RAS_KEY_N] == RAS_KEY_EVENT_UP
        && input_state->mods & RAS_KMOD_CTRL) {
        scene_state->normal_mode = (RasNormalMode)((scene_state->normal_mode + 1) % RAS_NORMAL_MODE_COUNT);

        ras_log_info("normal_mode: %s", repr_normal_mode(buffer, sizeof(buffer), scene_state->normal_mode));
    }
    if (input_state->keys[RAS_KEY_O] == RAS_KEY_EVENT_UP
        && input_state->mods & RAS_KMOD_CTRL
        && !(input_state->mods & RAS_KMOD_SHIFT)) {
        scene_state->polygon_mode = (scene_state->polygon_mode + 1) % 2;

        ras_log_info("polygon_mode: %s", scene_state->polygon_mode == RAS_POLYGON_WIREFRAME ? "wireframe" : "solid");
    }
    if (input_state->keys[RAS_KEY_O] == RAS_KEY_EVENT_UP
        && input_state->mods & RAS_KMOD_CTRL
        && input_state->mods & RAS_KMOD_SHIFT) {
        scene_state->polygon_outline_mode = (scene_state->polygon_outline_mode + 1) % RAS_POLYGON_OUTLINE_COUNT;

        ras_log_info(
            "polygon_outline_mode: %s",
            repr_polygon_outline_mode(buffer, sizeof(buffer), scene_state->polygon_outline_mode));
    }
    if (input_state->keys[RAS_KEY_G] == RAS_KEY_EVENT_UP
        && input_state->mods & RAS_KMOD_CTRL) {

        // Perform modulus with + 1 to ensure all flag values are iterated.
        scene_state->grid_mode = (scene_state->grid_mode + 1) % (RAS_GRID_MODE_COUNT + 1);

        ras_log_info("grid_mode: %s",
            repr_grid_mode(buffer, sizeof(buffer), scene_state->grid_mode));
    }

    if (input_state->keys[RAS_KEY_L] == RAS_KEY_EVENT_UP
        && input_state->mods & RAS_KMOD_CTRL) {
        scene_state->pipeline_mode = (scene_state->pipeline_mode + 1) % RAS_PIPELINE_MODE_COUNT;

        ras_log_info(
            "pipeline_mode: %s",
            scene_state->pipeline_mode == RAS_PIPELINE_MODE_OFF
                ? "off"
                : "default");
    }

    if (input_state->keys[RAS_KEY_F3] == RAS_KEY_EVENT_UP) {
        ui_state->layer_visible = !ui_state->layer_visible;
        ras_log_info("UI visible: %s", ui_state->layer_visible ? "true" : "false");
    }
    if (input_state->keys[RAS_KEY_BACKQUOTE] == RAS_KEY_EVENT_UP) {
        console_state->layer_visible = !console_state->layer_visible;
        ras_log_info("Console visible: %s", console_state->layer_visible ? "true" : "false");
    }

    if (input_state->keys[RAS_KEY_RETURN] == RAS_KEY_EVENT_UP) {
        ras_log_info("Return key.");
    }

    if (input_state->keys[RAS_KEY_F] == RAS_KEY_EVENT_UP
        && input_state->mods & RAS_KMOD_CTRL) {
        ras_log_flush();
    }
    ras_log_clear();

    input_state->current_frame = scene_state->current_frame;
}
