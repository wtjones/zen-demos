#include "rasgl/core/maths.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/repr.h"

void apply_unit_vector(Point2f* src, int angle, Point2f* dest)
{
    RasFixed xform_result[3];
    RasFixed c = RAS_COS(angle);
    RasFixed s = RAS_SIN(angle);

    RasFixed matrix[3][3] = {
        { c, -s, src->x },
        { s, c, src->y },
        { 0, 0, INT_32_TO_FIXED_16_16(1) }
    };
    RasFixed pos[3] = {
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

RasFixed core_dot_product(Point3f* v1, Point3f* v2)
{
    return mul_fixed_16_16_by_fixed_16_16(v1->x, v2->x)
        + mul_fixed_16_16_by_fixed_16_16(v1->y, v2->y)
        + mul_fixed_16_16_by_fixed_16_16(v1->z, v2->z);
}

void xform_to_world(
    Point2f* position,
    RasFixed angle_cos,
    RasFixed angle_sin,
    Point2f* source,
    Point2f* dest)
{
    // Rotate and translate to world coord
    RasFixed matrix[3][3] = {
        { angle_cos, -angle_sin, position->x },
        { angle_sin, angle_cos, position->y },
        { 0, 0, INT_32_TO_FIXED_16_16(1) }
    };

    RasFixed pos[3] = { source->x, source->y, INT_32_TO_FIXED_16_16(1) };
    RasFixed xform_result[3];
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
    RasFixed sw = INT_32_TO_FIXED_16_16(screen_width);
    RasFixed sh = INT_32_TO_FIXED_16_16(screen_height);
    RasFixed offset_x = (sw / (int32_t)2) - viewer_pos->x;
    RasFixed offset_y = (sh / (int32_t)2) - viewer_pos->z;

    // Scale and translate to screen coord
    RasFixed scale_matrix[3][3] = {
        { viewer_pos->y, 0, offset_x },
        { 0, viewer_pos->y, offset_y },
        { 0, 0, INT_32_TO_FIXED_16_16(1) }
    };
    RasFixed scale_result[3];
    RasFixed pos[3] = { source->x, source->z, INT_32_TO_FIXED_16_16(1) };
    mat_mul_3x3_3x1(scale_matrix, pos, scale_result);

    dest->x = scale_result[0];
    dest->y = scale_result[1];
}

void core_mul_vec_by_fixed_16_16(Point3f* v1, RasFixed f, Point3f* dest)
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

void core_translate_apply(RasFixed m[4][4], Point3f* v)
{
    m[0][3] = v->x;
    m[1][3] = v->y;
    m[2][3] = v->z;
}

void core_translate_init(RasFixed m[4][4], Point3f* v)
{
    mat_set_identity_4x4(m);
    core_translate_apply(m, v);
}

void core_rotate_x_apply(RasFixed m[4][4], int32_t angle)
{
    RasFixed c = RAS_COS(angle);
    RasFixed s = RAS_SIN(angle);

    m[1][1] = c;
    m[1][2] = -s;
    m[2][1] = s;
    m[2][2] = c;
}

void core_rotate_y_apply(RasFixed m[4][4], int32_t angle)
{
    RasFixed c = RAS_COS(angle);
    RasFixed s = RAS_SIN(angle);

    m[0][0] = c;
    m[0][2] = s;
    m[2][0] = -s;
    m[2][2] = c;
}

void core_rotate_z_apply(RasFixed m[4][4], int32_t angle)
{
    RasFixed c = RAS_COS(angle);
    RasFixed s = RAS_SIN(angle);

    m[0][0] = c;
    m[0][1] = -s;
    m[1][0] = s;
    m[1][1] = c;
}

void mat_rotate_x(RasFixed m[4][4], int32_t angle, RasFixed dest[4][4])
{
    RasFixed temp[4][4];
    mat_set_identity_4x4(temp);
    core_rotate_x_apply(temp, angle);
    mat_mul_4x4_4x4(temp, m, dest);
}

void mat_rotate_y(RasFixed m[4][4], int32_t angle, RasFixed dest[4][4])
{
    RasFixed temp[4][4];
    mat_set_identity_4x4(temp);
    core_rotate_y_apply(temp, angle);
    mat_mul_4x4_4x4(temp, m, dest);
}

void mat_rotate_z(RasFixed m[4][4], int32_t angle, RasFixed dest[4][4])
{
    RasFixed temp[4][4];
    mat_set_identity_4x4(temp);
    core_rotate_z_apply(temp, angle);
    mat_mul_4x4_4x4(temp, m, dest);
}

void mat_projection_init(
    RasFixed projection_matrix[4][4],
    RasFixed fov,
    RasFixed aspect_ratio,
    RasFixed near,
    RasFixed far)
{
    size_t fov_index = (size_t)((fov >> 16) - RAS_FOV_SCALE_OFFSET);
    fov_index = fov_index >= RAS_FOV_SCALE_STEPS
        ? RAS_FOV_SCALE_STEPS - 1
        : fov_index;
    fov_index = fov_index < 0 ? 0 : fov_index;
    RasFixed y_scale = fov_scale_table[fov_index];
    RasFixed x_scale = div_fixed_16_16_by_fixed_16_16(y_scale, aspect_ratio);

    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            projection_matrix[i][j] = 0;
        }
    }

    RasFixed far_x_near = mul_fixed_16_16_by_fixed_16_16(far, near);

    projection_matrix[0][0] = x_scale;
    projection_matrix[1][1] = y_scale;
    projection_matrix[2][2] = div_fixed_16_16_by_fixed_16_16(far + near, near - far);
    projection_matrix[2][3] = div_fixed_16_16_by_fixed_16_16(far_x_near, near - far);
    projection_matrix[3][2] = -RAS_FIXED_ONE;
}

void mat_mul_project(RasFixed projection_matrix[4][4], RasFixed v[4], RasFixed dest[4])
{
    char buffer[100];
    mat_mul_4x4_4x1(projection_matrix, v, dest);
    ras_log_buffer_trace("after proj matrix: %s\n", repr_mat_4x1(buffer, sizeof buffer, dest));

    // perspective divide
    if (dest[3] != 0) {
        dest[0] = div_fixed_16_16_by_fixed_16_16(dest[0], dest[3]);
        dest[1] = div_fixed_16_16_by_fixed_16_16(dest[1], dest[3]);
        dest[2] = div_fixed_16_16_by_fixed_16_16(dest[2], dest[3]);
    };
}

void core_view_space_to_clip_space(
    RasFixed projection_matrix[4][4],
    RasVector3f* view_space,
    RasVector4f* dest)
{
    RasFixed view_space_vec[4];
    RasFixed projected_vec[4];

    core_vector3f_to_4x1(view_space, view_space_vec);
    mat_mul_4x4_4x1(projection_matrix, view_space_vec, projected_vec);
    core_4x1_to_vector4f(projected_vec, dest);
}

void core_clip_space_to_ndc(RasFixed clip_space[4], RasFixed ndc_space[4])
{
    // Convert clip space coordinates to NDC by perspective divide.
    ndc_space[0] = div_fixed_16_16_by_fixed_16_16(clip_space[0], clip_space[3]);
    ndc_space[1] = div_fixed_16_16_by_fixed_16_16(clip_space[1], clip_space[3]);
    ndc_space[2] = div_fixed_16_16_by_fixed_16_16(clip_space[2], clip_space[3]);
    ndc_space[3] = RAS_FIXED_ONE;

#ifdef RAS_DEBUG_Z_DIVIDE
    static char buffer0[100];
    static char buffer1[100];
    ras_log_buffer("Divsors: x=%s/w=%s\n",
        repr_fixed_16_16(buffer0, sizeof buffer0, clip_space[0]),
        repr_fixed_16_16(buffer1, sizeof buffer1, clip_space[3]));
#endif
}

void core_clip_space_to_ndc_lut(RasFixed clip_space[4], RasFixed ndc_space[4])
{

    // Clamp the w value to the Z scale table range.
    RasFixed w = clip_space[3] < RAS_Z_SCALE_FIXED_MIN ? RAS_Z_SCALE_FIXED_MIN : clip_space[3];
    w = w > RAS_Z_SCALE_FIXED_MAX ? RAS_Z_SCALE_FIXED_MAX : w;

    // idx = (w - Z_MIN) * (Z_STEPS - 1) / (Z_MAX - Z_MIN);
    uint32_t lut_idx = (uint32_t)(((int64_t)(w - RAS_Z_SCALE_FIXED_MIN) * (RAS_Z_SCALE_STEPS - 1))
        / (RAS_Z_SCALE_FIXED_MAX - RAS_Z_SCALE_FIXED_MIN));

    RasFixed w_scale = z_scale_table[lut_idx]; // 1/w in fixed 16.16

    ndc_space[0] = mul_fixed_16_16_by_fixed_16_16(clip_space[0], w_scale);
    ndc_space[1] = mul_fixed_16_16_by_fixed_16_16(clip_space[1], w_scale);
    ndc_space[2] = mul_fixed_16_16_by_fixed_16_16(clip_space[2], w_scale);
    ndc_space[3] = RAS_FIXED_ONE;

#ifdef RAS_DEBUG_Z_DIVIDE
    static char buffer0[100];
    static char buffer1[100];
    static char buffer2[100];
    ras_log_buffer("Divsors: x=%s/w=%s\n  LUT index %u w_scale=%s",
        repr_fixed_16_16(buffer0, sizeof buffer0, clip_space[0]),
        repr_fixed_16_16(buffer1, sizeof buffer1, clip_space[3]),
        lut_idx,
        repr_fixed_16_16(buffer2, sizeof buffer2, w_scale));

    ras_log_buffer("Non-LUT: x=%s/y=%s\n",
        repr_fixed_16_16(buffer0, sizeof buffer0, ndc_space[0]),
        repr_fixed_16_16(buffer1, sizeof buffer1, ndc_space[1]));
#endif
}

void core_clip_space_to_ndc_lut_shift(RasFixed clip_space[4], RasFixed ndc_space[4])
{
    RasFixed w = clip_space[3];

    if (w < 0)
        w = 0;

    static const RasFixed w_max = (RasFixed)(RAS_Z_SCALE_STEPS << RAS_Z_SCALE_W_SHIFT);
    if (w >= w_max)
        w = w_max - 1;

    // LUT index based on shifted w value
    uint32_t lut_idx = w >> RAS_Z_SCALE_W_SHIFT;

    if (lut_idx >= RAS_Z_SCALE_STEPS)
        lut_idx = RAS_Z_SCALE_STEPS - 1;

    RasFixed w_scale = z_scale_table_shift[lut_idx]; // 1/w in fixed 16.16

    ndc_space[0] = mul_fixed_16_16_by_fixed_16_16(clip_space[0], w_scale);
    ndc_space[1] = mul_fixed_16_16_by_fixed_16_16(clip_space[1], w_scale);
    ndc_space[2] = mul_fixed_16_16_by_fixed_16_16(clip_space[2], w_scale);
    ndc_space[3] = RAS_FIXED_ONE;

#ifdef RAS_DEBUG_Z_DIVIDE
    static char buffer0[100];
    static char buffer1[100];
    static char buffer2[100];
    ras_log_buffer("Divsors: x=%s/w=%s\n  LUT index %u w_scale=%s",
        repr_fixed_16_16(buffer0, sizeof buffer0, clip_space[0]),
        repr_fixed_16_16(buffer1, sizeof buffer1, clip_space[3]),
        lut_idx,
        repr_fixed_16_16(buffer2, sizeof buffer2, w_scale));

    ras_log_buffer("Non-LUT: x=%s/y=%s\n",
        repr_fixed_16_16(buffer0, sizeof buffer0, ndc_space[0]),
        repr_fixed_16_16(buffer1, sizeof buffer1, ndc_space[1]));
#endif
}

void core_mat_normal_init(RasFixed mvt[4][4], RasFixed dest[4][4])
{
    mat_set_identity_4x4(dest);
    dest[0][0] = mvt[0][0];
    dest[0][1] = mvt[0][1];
    dest[0][2] = mvt[0][2];
    dest[0][3] = 0;

    dest[1][0] = mvt[1][0];
    dest[1][1] = mvt[1][1];
    dest[1][2] = mvt[1][2];
    dest[1][3] = 0;

    dest[2][0] = mvt[2][0];
    dest[2][1] = mvt[2][1];
    dest[2][2] = mvt[2][2];
    dest[2][3] = 0;

    dest[3][0] = 0;
    dest[3][1] = 0;
    dest[3][2] = 0;
    dest[3][3] = RAS_FIXED_ONE;
}

RasFixed core_get_vec_length(Point3f* vec)
{
    RasFixed length;

    // To find the length of a vector, simply add the square of its
    // components then take the square root of the result.
    RasFixed square = mul_fixed_16_16_by_fixed_16_16(vec->x, vec->x)
        + mul_fixed_16_16_by_fixed_16_16(vec->y, vec->y)
        + mul_fixed_16_16_by_fixed_16_16(vec->z, vec->z);
    assert(square > -1);
    return sqrt_fx16_16_to_fx16_16(square);
}

void core_normalize(Point3f* vec)
{
    RasFixed length = core_get_vec_length(vec);

    vec->x = div_fixed_16_16_by_fixed_16_16(vec->x, length);
    vec->y = div_fixed_16_16_by_fixed_16_16(vec->y, length);
    vec->z = div_fixed_16_16_by_fixed_16_16(vec->z, length);
}

void core_vector3f_to_4x1(RasVector3f* vec, RasFixed m[4])
{
    m[0] = vec->x;
    m[1] = vec->y;
    m[2] = vec->z;
    m[3] = INT_32_TO_FIXED_16_16(1);
}

void core_vector4f_to_4x1(RasVector4f* vec, RasFixed m[4])
{
    m[0] = vec->x;
    m[1] = vec->y;
    m[2] = vec->z;
    m[3] = vec->w;
}

void core_vector4f_to_vector2f(RasVector4f* vec4f, Point2f* vec2f)
{
    vec2f->x = vec4f->x;
    vec2f->y = vec4f->y;
}

void core_4x1_to_vector3f(RasFixed m[4], RasVector3f* vec)
{
    vec->x = m[0];
    vec->y = m[1];
    vec->z = m[2];
}

void core_4x1_to_vector4f(RasFixed m[4], RasVector4f* vec)
{
    vec->x = m[0];
    vec->y = m[1];
    vec->z = m[2];
    vec->w = m[3];
}

bool cmp_point3f(Point3f* p1, Point3f* p2)
{
    return p1->x == p2->x && p1->y == p2->y && p1->z == p2->z;
}

void core_min_vector3f(RasVector3f* v1, RasVector3f* v2, RasVector3f* dest)
{
    dest->x = v1->x < v2->x ? v1->x : v2->x;
    dest->y = v1->y < v2->y ? v1->y : v2->y;
    dest->z = v1->z < v2->z ? v1->z : v2->z;
}

void core_max_vector3f(RasVector3f* v1, RasVector3f* v2, RasVector3f* dest)
{
    dest->x = v1->x > v2->x ? v1->x : v2->x;
    dest->y = v1->y > v2->y ? v1->y : v2->y;
    dest->z = v1->z > v2->z ? v1->z : v2->z;
}

void core_sub_vector3f(RasVector3f* v1, RasVector3f* v2, RasVector3f* dest)
{
    dest->x = v1->x - v2->x;
    dest->y = v1->y - v2->y;
    dest->z = v1->z - v2->z;
}

bool core_point_in_rect(Point2f* p, Point2f* top_left, Point2f* bottom_right)
{
    return p->x >= top_left->x
        && p->x <= bottom_right->x
        && p->y >= top_left->y
        && p->y <= bottom_right->y;
}

void core_face_normal(RasVector3f* v1, RasVector3f* v2, RasVector3f* v3, RasVector3f* normal)
{
    RasVector3f edge1, edge2;
    core_sub_vector3f(v2, v1, &edge1);
    core_sub_vector3f(v3, v1, &edge2);
    core_cross_product(&edge1, &edge2, normal);
    core_normalize(normal);
}

RasOctant core_angle_to_octant(int32_t angle)
{
    return ((int32_t)round(angle / 45)) & 7;
}
