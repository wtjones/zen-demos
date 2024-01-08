#include "rasgl/core/camera.h"

void ras_camera_update(InputState* input_state, RasCamera* camera)
{
    int32_t viewer_angle_prev = camera->angle;
    int32_t delta_angle = 0;
    if (input_state->keys[RAS_KEY_Q] == 1 || input_state->keys[RAS_KEY_LEFT]) {
        delta_angle = ROTATION_SPEED;
    }
    if (input_state->keys[RAS_KEY_E] == 1 || input_state->keys[RAS_KEY_RIGHT]) {
        delta_angle = -ROTATION_SPEED;
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
        mul_fixed_16_16_by_fixed_16_16(world_vector.x - camera->position.x, VIEWER_SPEED),
        mul_fixed_16_16_by_fixed_16_16(world_vector.y - camera->position.z, VIEWER_SPEED)
    };

    Point3f viewer_pos_prev;
    memcpy(&viewer_pos_prev, &camera->position, sizeof camera->position);

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
    if (input_state->keys[RAS_KEY_EQUALS] == 1) {
        camera->position.y += ZOOM_SPEED;
    }
    if (input_state->keys[RAS_KEY_MINUS] == 1) {
        camera->position.y -= ZOOM_SPEED;
    }
    if (input_state->keys[RAS_KEY_P] == RAS_KEY_EVENT_UP) {
        camera->projection_mode = camera->projection_mode == RAS_PERSPECTIVE_MATRIX
            ? RAS_ORTHO_MATRIX
            : RAS_PERSPECTIVE_MATRIX;
    }
    if (input_state->keys[RAS_KEY_LEFTBRACKET] == 1) {
        camera->fov -= FOV_SPEED;
        ras_log_info("FOV: %f\n", camera->fov);
    }
    if (input_state->keys[RAS_KEY_RIGHTBRACKET] == 1) {
        camera->fov += FOV_SPEED;
        ras_log_info("FOV: %f\n", camera->fov);
    }

    if (camera->angle != viewer_angle_prev) {
        ras_log_info("camera->angle: %d\n", camera->angle);
    }
}