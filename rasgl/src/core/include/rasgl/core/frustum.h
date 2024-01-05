#ifndef FRUSTUM_H
#define FRUSTUM_H

#include "fixed_maths.h"
#include "maths.h"
#include "matrix.h"
#include "plane.h"
#define _USE_MATH_DEFINES
#include <math.h>
#include <stdbool.h>
#include <stdint.h>

#define FRUSTUM_PLANES 6
#define FRUSTUM_POINTS 8

enum {
    PLANE_LEFT,
    PLANE_RIGHT,
    PLANE_BOTTOM,
    PLANE_TOP,
    PLANE_NEAR,
    PLANE_FAR
};

typedef struct RasFrustum {
    Plane planes[FRUSTUM_PLANES];
    Point3f points[FRUSTUM_POINTS];
} RasFrustum;

/**
 * Extract the frustum from the combined view and projection matrices.
 */
void core_frustum_init(int32_t view_projection_matrix[4][4], RasFrustum* frustum);

char* core_repr_frustum(char* buffer, size_t count, RasFrustum* frustum);

#endif
