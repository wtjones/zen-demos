#ifndef FRUSTUM_H
#define FRUSTUM_H

#include "fixed_maths.h"
#include "maths.h"
#include "matrix.h"
#include "plane.h"
#include <stdbool.h>
#include <stdint.h>

#define FRUSTUM_PLANES 6
#define FRUSTUM_POINTS 8

typedef enum RasFrustumPlane {
    PLANE_LEFT,
    PLANE_RIGHT,
    PLANE_BOTTOM,
    PLANE_TOP,
    PLANE_NEAR,
    PLANE_FAR
} RasFrustumPlane;

typedef struct RasFrustum {
    RasPlane planes[FRUSTUM_PLANES];
    Point3f points[FRUSTUM_POINTS];
} RasFrustum;

/**
 * Bitmask of frustum plane indices
 */
typedef uint8_t RasClipFlags;

static inline RasClipFlags core_to_clip_flag(RasFrustumPlane plane)
{
    return 1 << plane;
}

/**
 * Extract the frustum from the combined view and projection matrices.
 */
void core_frustum_init(RasFixed view_projection_matrix[4][4], RasFrustum* frustum);

bool core_point_in_frustum(RasFrustum* frustum, RasVector3f* point);

char* core_repr_frustum(char* buffer, size_t count, RasFrustum* frustum);

/**
 * Returns bitmask of planes where the point is outside.
 */
RasClipFlags core_point_in_frustum_planes(RasFrustum* frustum, RasVector3f* point);

#endif
