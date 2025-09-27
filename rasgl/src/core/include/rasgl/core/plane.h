#ifndef PLANE_H
#define PLANE_H

#include "debug.h"
#include "maths.h"

typedef enum RasPlaneSideResult {
    RAS_PLANE_SIDE_A,
    RAS_PLANE_SIDE_B,
    RAS_PLANE_SIDE_EQUAL
} RasPlaneSideResult;

typedef struct RasPlane {
    Point3f normal;
    RasFixed distance;
} RasPlane;

/**
 * Returns true if the line intersects the plane.
 */
bool core_plane_line_intersect(RasPlane* plane, Point3f* v1, Point3f* v2);

RasPlaneSideResult core_plane_vector_side(RasPlane* plane, Point3f* v1);

RasPlaneSideResult core_plane_vector_side(RasPlane* plane, Point3f* v1);

RasPlaneSideResult core_plane_vector4f_side(RasPlane* plane, RasVector4f* v1);

bool core_get_3_plane_intersection(RasPlane* p1, RasPlane* p2, RasPlane* p3, Point3f* point);

char* core_repr_plane(char* buffer, size_t count, RasPlane* plane);

#endif
