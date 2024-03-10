#ifndef MATHS_H
#define MATHS_H

#include "fixed_maths.h"
#include "fpsqrt/fpsqrt.h"
#include "maths_tables.h"
#include "matrix.h"
#define _USE_MATH_DEFINES
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>

#define MAX_SHAPE_VERTICES 10

typedef struct Point2i {
    int32_t x;
    int32_t y;
} Point2i;

typedef struct Point2f {
    RasFixed x;
    RasFixed y;
} Point2f;

typedef struct Point3f {
    RasFixed x;
    RasFixed y;
    RasFixed z;
} Point3f;

typedef struct Point3f RasVector3f;

typedef struct RasVector4f {
    RasFixed x;
    RasFixed y;
    RasFixed z;
    RasFixed w;
} RasVector4f;

extern RasFixed cos_table[360];
extern RasFixed sin_table[360];

void init_math_lookups();
void apply_unit_vector(Point2f* src, int angle, Point2f* dest);
int64_t cross(Point2f* v1, Point2f* v2);
void core_cross_product(Point3f* v1, Point3f* v2, Point3f* result);
RasFixed core_dot_product(Point3f* v1, Point3f* v2);
void xform_to_world(
    Point2f* position,
    RasFixed angle_cos,
    RasFixed angle_sin,
    Point2f* point,
    Point2f* dest);

/**
 * @brief Scale and translate world space to view space
 *
 * @param screen_width
 * @param screen_height
 * @param viewer_pos
 * @param source
 * @param dest
 */
void xform_to_screen(
    int screen_width,
    int screen_height,
    Point3f* viewer_pos,
    Point3f* source,
    Point2f* dest);

/**
 * Multiply vector by a fixed point value
 */
void core_mul_vec_by_fixed_16_16(Point3f* v1, RasFixed f, Point3f* dest);

/**
 * Multiply vector by a vector
 */
void core_mul_vec_by_vec(Point3f* v1, Point3f* v2, Point3f* dest);

void core_translate_apply(RasFixed m[4][4], Point3f* v);

/**
 * Inits a translation matrix by vector
 */
void core_translate_init(RasFixed m[4][4], Point3f* v);

void core_rotate_x_apply(RasFixed m[4][4], int32_t angle);

void core_rotate_y_apply(RasFixed m[4][4], int32_t angle);

void core_rotate_z_apply(RasFixed m[4][4], int32_t angle);

/**
 * Rotate given matrix by angle and store in dest
 */
void mat_rotate_x(RasFixed m[4][4], int32_t angle, RasFixed dest[4][4]);

/**
 * Rotate given matrix by angle and store in dest
 */
void mat_rotate_y(RasFixed m[4][4], int32_t angle, RasFixed dest[4][4]);

/**
 * Rotate given matrix by angle and store in dest
 */
void mat_rotate_z(RasFixed m[4][4], int32_t angle, RasFixed dest[4][4]);

void mat_projection_init(RasFixed projection_matrix[4][4], float fov, float aspect_ratio, float near, float far);

/**
 * Multiply a 4x1 vector with a project matrix and perform perspective divide on
 * the carried over z -> w.
 */
void mat_mul_project(RasFixed projection_matrix[4][4], RasFixed v[4], RasFixed dest[4]);

void core_vector3f_to_4x1(RasVector3f* vec, RasFixed m[4]);
void core_4x1_to_vector3f(RasFixed m[4], RasVector3f* vec);

/**
 * @brief Perform memberwise comparison of Point3f
 */
bool cmp_point3f(Point3f* p1, Point3f* p2);

RasFixed core_get_vec_length(Point3f* vec);

void core_normalize(Point3f* vec);

void core_mat_mul_4x4_vec3f(RasFixed projection_matrix[4][4], RasVector3f* vec, RasVector3f* dest);

void core_min_vector3f(RasVector3f* v1, RasVector3f* v2, RasVector3f* dest);
void core_max_vector3f(RasVector3f* v1, RasVector3f* v2, RasVector3f* dest);

#endif
