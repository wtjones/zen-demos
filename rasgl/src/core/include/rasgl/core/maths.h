#ifndef MATHS_H
#define MATHS_H

#include "fixed_maths.h"
#include "maths_tables.h"
#include "matrix.h"
#define _USE_MATH_DEFINES
#include <math.h>
#include <stdbool.h>
#include <stdint.h>

#define MAX_SHAPE_VERTICES 10

typedef struct Point2i {
    int32_t x;
    int32_t y;
} Point2i;

typedef struct Point2f {
    int32_t x;
    int32_t y;
} Point2f;

typedef struct Point3f {
    int32_t x;
    int32_t y;
    int32_t z;
} Point3f;

extern int32_t cos_table[360];
extern int32_t sin_table[360];

void init_math_lookups();
void apply_unit_vector(Point2f* src, int angle, Point2f* dest);
int64_t cross(Point2f* v1, Point2f* v2);
void xform_to_world(
    Point2f* position,
    int32_t angle_cos,
    int32_t angle_sin,
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
 * Inits a translation matrix by vector
 */
void mat_translate_init(int32_t m[4][4], Point3f* v);

/**
 * Rotate given matrix by angle and store in dest
 */
void mat_rotate_y(int32_t m[4][4], int32_t angle, int32_t dest[4][4]);

void mat_projection_init(int32_t projection_matrix[4][4], float fov, float aspect_ratio, float near, float far);

/**
 * Multiply a 4x1 vector with a project matrix and perform perspective divide on
 * the carried over z -> w.
 */
void mat_mul_project(int32_t projection_matrix[4][4], int32_t v[4], int32_t dest[4]);

/**
 * @brief Perform memberwise comparison of Point3f
 */
bool cmp_point3f(Point3f* p1, Point3f* p2);

#endif
