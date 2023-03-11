#include "maths.h"

extern int32_t cos_table[360];
extern int32_t sin_table[360];


void apply_unit_vector(Point2f* src, int angle, Point2f* dest)
{
    int32_t xform_result[3];
    int32_t c = cos_table[angle];
    int32_t s = sin_table[angle];

    int32_t matrix[3][3] = {
        { c, -s, src->x },
        { s, c, src->y },
        { 0, 0, INT_32_TO_FIXED_16_16(1) }
    };
    int32_t pos[3] = {
        0.0,
        INT_32_TO_FIXED_16_16(1),
        INT_32_TO_FIXED_16_16(1)
    };
    mat_mul_3x3_3x1(matrix, pos, xform_result);

    dest->x = xform_result[0];
    dest->y = xform_result[1];
}

/**
 * @brief v1->x * v2->y - v1->y * v2->x
 *
 * @param v1
 * @param v2
 * @return int64_t
 */
int64_t cross(Point2f* v1, Point2f* v2)
{
    int64_t xy = mul_fixed_16_16_to_fixed_32_32(v1->x, v2->y);
    int64_t yx = mul_fixed_16_16_to_fixed_32_32(v1->y, v2->x);

    return xy - yx;
}

bool point_in_polygon(
    Point2f* p, Shape* shape, Point2f* collide_p1, Point2f* collide_p2)
{
    for (int v = 0; v < shape->num_vertices; v++) {
        bool last = v == shape->num_vertices - 1;
        int next_index = last ? 0 : v + 1;
        Point2f* side_p1 = &shape->vertices[v];
        Point2f* side_p2 = &shape->vertices[next_index];

        Point2f v1 = {
            .x = side_p2->x - side_p1->x,
            .y = side_p2->y - side_p1->y
        };

        Point2f v2 = {
            .x = p->x - side_p1->x,
            .y = p->y - side_p1->y
        };

        int64_t facing = cross(&v1, &v2);

        if (facing < 0) {
            collide_p1->x = side_p1->x;
            collide_p1->y = side_p1->y;
            collide_p2->x = side_p2->x;
            collide_p2->y = side_p2->y;

            return true;
        }
    }
    return false;
}

void xform_to_world(
    Point2f* position,
    int32_t angle_cos,
    int32_t angle_sin,
    Point2f* source,
    Point2f* dest)
{
    // Rotate and translate to world coord
    int32_t matrix[3][3] = {
        { angle_cos, -angle_sin, position->x },
        { angle_sin, angle_cos, position->y },
        { 0, 0, INT_32_TO_FIXED_16_16(1) }
    };

    int32_t pos[3] = { source->x, source->y, INT_32_TO_FIXED_16_16(1) };
    int32_t xform_result[3];
    mat_mul_3x3_3x1(matrix, pos, xform_result);
    dest->x = xform_result[0];
    dest->y = xform_result[1];
}

void xform_to_screen(
    int screen_width,
    int screen_height,
    Point3f* viewer_pos,
    Point2f* source,
    Point2f* dest)
{
    int32_t sw = INT_32_TO_FIXED_16_16(screen_width);
    int32_t sh = INT_32_TO_FIXED_16_16(screen_height);
    int32_t offset_x = (sw / (int32_t)2) - viewer_pos->x;
    int32_t offset_y = (sh / (int32_t)2) - viewer_pos->y;

    // Scale and translate to screen coord
    int32_t scale_matrix[3][3] = {
        { viewer_pos->z, 0, offset_x },
        { 0, viewer_pos->z, offset_y },
        { 0, 0, INT_32_TO_FIXED_16_16(1) }
    };
    int32_t scale_result[3];
    int32_t pos[3] = { source->x, source->y, INT_32_TO_FIXED_16_16(1) };
    mat_mul_3x3_3x1(scale_matrix, pos, scale_result);

    dest->x = scale_result[0];
    dest->y = scale_result[1];
}
