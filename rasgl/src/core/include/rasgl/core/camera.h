#ifndef CAMERA_H
#define CAMERA_H

#include "frustum.h"
#include "graphics.h"
#include "input.h"
#include "maths.h"

#define ZOOM_SPEED float_to_fixed_16_16(.05)
#define ROTATION_SPEED 1
#define FOV_SPEED 2.0f
#define VIEWER_SPEED float_to_fixed_16_16(.5)
#define PROJECTION_RATIO -float_to_fixed_16_16(2.0)

typedef struct RasCamera {
    RasVector3f position;
    int32_t angle;
    float fov;
    RasProjectionMode projection_mode;
    RasFrustum frustum;
} RasCamera;

void ras_camera_update(InputState* input_state, RasCamera* camera);

#endif