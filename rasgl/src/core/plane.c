#include "rasgl/core/plane.h"
#include "rasgl/core/repr.h"

/**
 *
 * quote from http://nate.scuzzy.net/docs/normals/
 * "If we have a point p1, we can find out its position with respect to the plane
 * by plugging it into the plane equation:
 *
 * value = (A * p1.x) + (B * p1.y) + (C * p1.z) + D
 *
 * After plugging the point into the plane equation we get a number, value,
 * which provides a great amount of information to us. If value is less than 0,
 * the point is behind the plane. If value is greater than zero, the point is in
 * front of the plane."
 */
bool core_plane_line_intersect(Plane* plane, Point3f* v1, Point3f* v2)
{
    int32_t v1pos, v2pos;

    v1pos = mul_fixed_16_16_by_fixed_16_16(plane->normal.x, v1->x)
        + mul_fixed_16_16_by_fixed_16_16(plane->normal.y, v1->y)
        + mul_fixed_16_16_by_fixed_16_16(plane->normal.z, v1->z)
        + plane->distance;
    v2pos = mul_fixed_16_16_by_fixed_16_16(plane->normal.x, v2->x)
        + mul_fixed_16_16_by_fixed_16_16(plane->normal.y, v2->y)
        + mul_fixed_16_16_by_fixed_16_16(plane->normal.z, v2->z)
        + plane->distance;

    if (v1pos > 0 && v2pos > 0)
        return false;
    else if (v1pos < 0 && v2pos < 0)
        return false;
    else
        return true;
}

/**
 *
 * v = ( a.n.Cross ( b.n ) * -d - b.n.Cross ( n )  * a.d - n.Cross ( a.n ) * b.d ) / denom;
 *
 *  part1: a.n.Cross ( b.n ) * -d
 *  part2: b.n.Cross ( n )  * a.d
 *  part3: n.Cross ( a.n ) * b.d
 *
 *  v = (part1 - part2 - part3) / denom;
 */
bool core_get_3_plane_intersection(Plane* p1, Plane* p2, Plane* p3, Point3f* point)
{
    Point3f crossTemp;
    Point3f cross1, cross2, cross3;
    Point3f part1, part2, part3;

    core_cross_product(&p2->normal, &p3->normal, &cross1);
    int32_t denom = core_dot_product(&p1->normal, &cross1);

    if (denom == 0)
        return false;

    core_cross_product(&p3->normal, &p1->normal, &cross2);
    core_cross_product(&p1->normal, &p2->normal, &cross3);

    core_mul_vec_by_fixed_16_16(&cross1, -p1->distance, &part1);
    core_mul_vec_by_fixed_16_16(&cross2, p2->distance, &part2);
    core_mul_vec_by_fixed_16_16(&cross3, p3->distance, &part3);

    point->x = div_fixed_16_16_by_fixed_16_16(part1.x - part2.x - part3.x, denom);
    point->y = div_fixed_16_16_by_fixed_16_16(part1.y - part2.y - part3.y, denom);
    point->z = div_fixed_16_16_by_fixed_16_16(part1.z - part2.z - part3.z, denom);

    return true;
}

char* core_repr_plane(char* buffer, size_t count, Plane* plane)
{
    char buffer2[255];
    char buffer3[255];
    char buffer4[255];

    buffer[0] = '\0';
    snprintf(
        buffer2,
        sizeof buffer2,
        "[\n    normal: %s\n    dist: %s\n]",
        repr_point3f(buffer3, sizeof buffer3, &plane->normal),
        repr_fixed_16_16(buffer4, sizeof buffer4, plane->distance));
    strcat(buffer, buffer2);
    return buffer;
}