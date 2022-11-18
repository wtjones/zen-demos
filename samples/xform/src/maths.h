#ifndef MATHS_H
#define MATHS_H

#include "matrix.h"
#define _USE_MATH_DEFINES
#include <math.h>
#include <stdbool.h>

#ifndef M_PI
#    define M_PI 3.14159265358979323846
#endif

#define MAX_SHAPE_VERTICES 10

typedef struct Point2f {
    float x;
    float y;
} Point2f;

typedef struct Point3f {
    float x;
    float y;
    float z;
} Point3f;

typedef struct Shape {
    int num_vertices;
    Point2f vertices[MAX_SHAPE_VERTICES];
} Shape;

extern float cos_table[360];
extern float sin_table[360];

void init_math_lookups();
void apply_unit_vector(Point2f* src, int angle, Point2f* dest);
float cross(Point2f* v1, Point2f* v2);
bool point_in_polygon(
    Point2f* p, Shape* shape, Point2f* collide_p1, Point2f* collide_p2);
void xform_to_world(
    Point2f* position,
    float angle_cos,
    float angle_sin,
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
    Point2f* source,
    Point2f* dest);
#endif
