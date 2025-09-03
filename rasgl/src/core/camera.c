#include "rasgl/core/camera.h"
#include "rasgl/core/event.h"
#include "rasgl/core/repr.h"

void ras_camera_update(RasCamera* camera, InputState* input_state)
{
    RasCamera camera_prev;
    memcpy(&camera_prev, camera, sizeof camera_prev);
    int32_t delta_angle = 0;

    if (input_state->keys[RAS_KEY_Q] == 1) {
        delta_angle = RAS_ROTATION_SPEED;
    }
    if (input_state->keys[RAS_KEY_E] == 1) {
        delta_angle = -RAS_ROTATION_SPEED;
    }

    camera->angle = (camera->angle + delta_angle) % 360;
    if (camera->angle < 0) {
        camera->angle += 360;
    };

    Point2f origin = { .x = 0, .y = 0 };
    Point2f vector;
    apply_unit_vector(&origin, camera->angle, &vector);

    // translate direction vector to world space
    Point2f world_vector = { camera->position.x + vector.x, camera->position.z + vector.y };

    Point2f delta = {
        mul_fixed_16_16_by_fixed_16_16(world_vector.x - camera->position.x, RAS_VIEWER_SPEED),
        mul_fixed_16_16_by_fixed_16_16(world_vector.y - camera->position.z, RAS_VIEWER_SPEED)
    };

    Point3f viewer_pos_prev;
    memcpy(&viewer_pos_prev, &camera->position, sizeof camera->position);

    if (input_state->mods == RAS_KMOD_NONE) {
        if (input_state->keys[RAS_KEY_W] == 1) {
            camera->position.z += delta.y;
            camera->position.x += delta.x;
        }
        if (input_state->keys[RAS_KEY_A] == 1) {
            camera->position.z -= delta.x;
            camera->position.x += delta.y;
        }
        if (input_state->keys[RAS_KEY_S] == 1) {
            camera->position.z -= delta.y;
            camera->position.x -= delta.x;
        }
        if (input_state->keys[RAS_KEY_D] == 1) {
            camera->position.z += delta.x;
            camera->position.x -= delta.y;
        }
        if (input_state->keys[RAS_KEY_Z] == 1) {
            camera->position.y += RAS_VIEWER_SPEED;
        }
        if (input_state->keys[RAS_KEY_C] == RAS_KEY_EVENT_DOWN) {
            camera->position.y -= RAS_VIEWER_SPEED;
        }
        if (input_state->keys[RAS_KEY_EQUALS] == 1) {
            camera->position.y += RAS_ZOOM_SPEED;
        }
        if (input_state->keys[RAS_KEY_MINUS] == 1) {
            camera->position.y -= RAS_ZOOM_SPEED;
        }
        if (input_state->keys[RAS_KEY_LEFTBRACKET] == 1) {
            camera->fov -= RAS_FOV_SPEED;
            ras_log_debug("FOV: %f\n", camera->fov);
        }
        if (input_state->keys[RAS_KEY_RIGHTBRACKET] == 1) {
            camera->fov += RAS_FOV_SPEED;
            ras_log_debug("FOV: %f\n", camera->fov);
        }
    }

    if (input_state->keys[RAS_KEY_LEFTBRACKET] == RAS_KEY_EVENT_DOWN
        && input_state->mods & RAS_KMOD_CTRL
        && !(input_state->mods & RAS_KMOD_SHIFT)) {
        camera->far -= (camera->far - RAS_PLANE_SPEED) < RAS_FAR_PLANE_MIN
            ? camera->far - RAS_FAR_PLANE_MIN
            : RAS_PLANE_SPEED;
    }
    if (input_state->keys[RAS_KEY_RIGHTBRACKET] == RAS_KEY_EVENT_DOWN
        && input_state->mods & RAS_KMOD_CTRL
        && !(input_state->mods & RAS_KMOD_SHIFT)) {
        camera->far += RAS_PLANE_SPEED;
    }

    if (input_state->keys[RAS_KEY_LEFTBRACKET] == RAS_KEY_EVENT_DOWN
        && input_state->mods & RAS_KMOD_CTRL
        && (input_state->mods & RAS_KMOD_SHIFT)) {
        camera->near -= (camera->near - RAS_PLANE_SPEED) < RAS_NEAR_PLANE_MIN
            ? camera->near - RAS_NEAR_PLANE_MIN
            : RAS_PLANE_SPEED;
    }
    if (input_state->keys[RAS_KEY_RIGHTBRACKET] == RAS_KEY_EVENT_DOWN
        && input_state->mods & RAS_KMOD_CTRL
        && (input_state->mods & RAS_KMOD_SHIFT)) {
        camera->near += RAS_PLANE_SPEED;
    }

    if (input_state->keys[RAS_KEY_P] == RAS_KEY_EVENT_UP
        && input_state->mods & RAS_KMOD_CTRL) {
        camera->projection_mode = camera->projection_mode == RAS_PERSPECTIVE_MATRIX
            ? RAS_ORTHO_MATRIX
            : RAS_PERSPECTIVE_MATRIX;
    }
    bool changed = (memcmp(&camera_prev, camera, sizeof camera_prev) != 0);
    camera->last_changed_frame = changed
        ? input_state->current_frame
        : camera->last_changed_frame;

    if (changed) {
        char buffer[255];
        ras_log_buffer_ex(
            RAS_EVENT_CM_CHANGE, "camera: %s", repr_camera(buffer, sizeof buffer, camera));
    }
}

void ras_camera_projection_init(RasCamera* camera, RasFixed projection_matrix[4][4])
{
    mat_projection_init(
        projection_matrix,
        camera->fov,
        camera->aspect_ratio,
        camera->near,
        camera->far);
}

void ras_camera_world_view_init(RasCamera* camera, RasFixed world_view_matrix[4][4])
{
    RasFixed translate_to_viewer[4][4];

    int32_t angle = (camera->angle + 180) % 360;
    if (angle < 0) {
        angle += 360;
    }

    // Combine world to viewer translate and rotate operations
    Point3f trans_pos = {
        -camera->position.x,
        -camera->position.y,
        -camera->position.z
    };
    core_translate_init(translate_to_viewer, &trans_pos);
    mat_rotate_y(translate_to_viewer, angle, world_view_matrix);
}
