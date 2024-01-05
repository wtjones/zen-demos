#ifndef PLANE_H
#define PLANE_H

#include "debug.h"
#include "maths.h"

typedef struct Plane {
    Point3f normal;
    int32_t distance;
} Plane;

/**
 * Returns true if the line intersects the plane.
 */
bool core_plane_line_intersect(Plane* plane, Point3f* v1, Point3f* v2);

bool core_get_3_plane_intersection(Plane* p1, Plane* p2, Plane* p3, Point3f* point);

char* core_repr_plane(char* buffer, size_t count, Plane* plane);

#endif
