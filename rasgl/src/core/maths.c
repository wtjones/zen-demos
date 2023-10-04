#include "rasgl/core/maths.h"
#include "rasgl/core/debug.h"

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

void core_cross_product(Point3f* v1, Point3f* v2, Point3f* result)
{
    result->x = mul_fixed_16_16_by_fixed_16_16(v1->y, v2->z) - mul_fixed_16_16_by_fixed_16_16(v1->z, v2->y);
    result->y = mul_fixed_16_16_by_fixed_16_16(v1->z, v2->x) - mul_fixed_16_16_by_fixed_16_16(v1->x, v2->z);
    result->z = mul_fixed_16_16_by_fixed_16_16(v1->x, v2->y) - mul_fixed_16_16_by_fixed_16_16(v1->y, v2->x);
}

int32_t core_dot_product(Point3f* v1, Point3f* v2)
{
    return mul_fixed_16_16_by_fixed_16_16(v1->x, v2->x)
        + mul_fixed_16_16_by_fixed_16_16(v1->y, v2->y)
        + mul_fixed_16_16_by_fixed_16_16(v1->z, v2->z);
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

void core_mul_vec_by_fixed_16_16(Point3f* v1, int32_t f, Point3f* dest)
{
    dest->x = mul_fixed_16_16_by_fixed_16_16(v1->x, f);
    dest->y = mul_fixed_16_16_by_fixed_16_16(v1->y, f);
    dest->z = mul_fixed_16_16_by_fixed_16_16(v1->z, f);
}

void core_mul_vec_by_vec(Point3f* v1, Point3f* v2, Point3f* dest)
{
    dest->x = mul_fixed_16_16_by_fixed_16_16(v1->x, v2->x);
    dest->y = mul_fixed_16_16_by_fixed_16_16(v1->y, v2->y);
    dest->z = mul_fixed_16_16_by_fixed_16_16(v1->z, v2->z);
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

void mat_projection_init(int32_t projection_matrix[4][4], float fov, float aspect_ratio, float near, float far)
{
    double d2r = M_PI / 180.0;
    double y_scale = 1.0 / tan(d2r * fov / 2); // FOV scaling factor
    double x_scale = y_scale / aspect_ratio;

    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            projection_matrix[i][j] = 0;
        }
    }

    projection_matrix[0][0] = float_to_fixed_16_16(x_scale);
    projection_matrix[1][1] = float_to_fixed_16_16(-y_scale);
    projection_matrix[2][2] = float_to_fixed_16_16((far + near) / (near - far));
    projection_matrix[2][3] = float_to_fixed_16_16((far * near) / (near - far));
    projection_matrix[3][2] = float_to_fixed_16_16(-1.0f);
}

void mat_mul_project(int32_t projection_matrix[4][4], int32_t v[4], int32_t dest[4])
{
    char buffer[100];
    mat_mul_4x4_4x1(projection_matrix, v, dest);
    ras_log_trace("after proj matrix: %s\n", repr_mat_4x1(buffer, sizeof buffer, dest));

    // perspective divide
    if (dest[3] != 0) {
        dest[0] = div_fixed_16_16_by_fixed_16_16(dest[0], dest[3]);
        dest[1] = div_fixed_16_16_by_fixed_16_16(dest[1], dest[3]);
        dest[2] = div_fixed_16_16_by_fixed_16_16(dest[2], dest[3]);
    };
}

int32_t core_get_vec_length(Point3f* vec)
{
    int32_t length;

    // To find the length of a vector, simply add the square of its
    // components then take the square root of the result.
    int32_t square = mul_fixed_16_16_by_fixed_16_16(vec->x, vec->x)
        + mul_fixed_16_16_by_fixed_16_16(vec->y, vec->y)
        + mul_fixed_16_16_by_fixed_16_16(vec->z, vec->z);
    assert(square > -1);
    return sqrt_fx16_16_to_fx16_16(square);
}

void core_normalize(Point3f* vec)
{
    int32_t length = core_get_vec_length(vec);

    vec->x = div_fixed_16_16_by_fixed_16_16(vec->x, length);
    vec->y = div_fixed_16_16_by_fixed_16_16(vec->y, length);
    vec->z = div_fixed_16_16_by_fixed_16_16(vec->z, length);
}

bool cmp_point3f(Point3f* p1, Point3f* p2)
{
    return p1->x == p2->x && p1->y == p2->y && p1->z == p2->z;
}
