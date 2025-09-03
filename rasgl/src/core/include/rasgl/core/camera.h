#ifndef CAMERA_H
#define CAMERA_H

#include "frustum.h"
#include "graphics.h"
#include "input.h"
#include "maths.h"

#define RAS_ZOOM_SPEED float_to_fixed_16_16(.05)
#define RAS_ROTATION_SPEED 1
#define RAS_FOV_SPEED 2.0f
#define RAS_PLANE_SPEED 0.1f
#define RAS_NEAR_PLANE_MIN 0.1f
#define RAS_FAR_PLANE_MIN 1.0f
#define RAS_VIEWER_SPEED float_to_fixed_16_16(0.025)
#define RAS_PROJECTION_RATIO -float_to_fixed_16_16(2.0)

typedef struct RasCamera {
    RasVector3f position;
    int32_t angle;
    float fov;
    float aspect_ratio; // (width/height)
    float near;         // Near clipping plane
    float far;          // Far clipping plane
    RasProjectionMode projection_mode;
    RasFixed projection_matrix[4][4];
    uint32_t last_changed_frame; // frame counter
} RasCamera;

void ras_camera_update(RasCamera* camera, InputState* input_state);

void ras_camera_projection_init(RasCamera* camera, RasFixed projection_matrix[4][4]);

/**
 * Combine world to viewer translate and rotate operations.
 */
void ras_camera_world_view_init(RasCamera* camera, RasFixed world_view_matrix[4][4]);

#endif
