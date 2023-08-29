#include "rasgl/core/maths.h"

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
        0,
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
    Point3f* source,
    Point2f* dest)
{
    int32_t sw = INT_32_TO_FIXED_16_16(screen_width);
    int32_t sh = INT_32_TO_FIXED_16_16(screen_height);
    int32_t offset_x = (sw / (int32_t)2) - viewer_pos->x;
    int32_t offset_y = (sh / (int32_t)2) - viewer_pos->z;

    // Scale and translate to screen coord
    int32_t scale_matrix[3][3] = {
        { viewer_pos->y, 0, offset_x },
        { 0, viewer_pos->y, offset_y },
        { 0, 0, INT_32_TO_FIXED_16_16(1) }
    };
    int32_t scale_result[3];
    int32_t pos[3] = { source->x, source->z, INT_32_TO_FIXED_16_16(1) };
    mat_mul_3x3_3x1(scale_matrix, pos, scale_result);

    dest->x = scale_result[0];
    dest->y = scale_result[1];
}

void mat_translate_init(int32_t m[4][4], Point3f* v)
{
    mat_set_identity_4x4(m);
    m[0][3] = v->x;
    m[1][3] = v->y;
    m[2][3] = v->z;
}

void mat_rotate_y(int32_t m[4][4], int32_t angle, int32_t dest[4][4])
{
    int32_t temp[4][4];
    mat_set_identity_4x4(temp);
    int32_t c = cos_table[angle];
    int32_t s = sin_table[angle];

    temp[0][0] = c;
    temp[0][2] = s;
    temp[2][0] = -s;
    temp[2][2] = c;
    mat_mul_4x4_4x4(temp, m, dest);
}

bool cmp_point3f(Point3f* p1, Point3f* p2)
{
    return p1->x == p2->x && p1->y == p2->y && p1->z == p2->z;
}
