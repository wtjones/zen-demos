#ifndef MATHS_H
#define MATHS_H

#include "matrix.h"
#define _USE_MATH_DEFINES
#include <math.h>

#ifndef M_PI
#    define M_PI 3.14159265358979323846
#endif

typedef struct Point2f {
    float x;
    float y;
} Point2f;

typedef struct Point3f {
    float x;
    float y;
    float z;
} Point3f;

extern float cos_table[360];
extern float sin_table[360];

void init_math_lookups();
void apply_unit_vector(Point2f* src, int angle, Point2f* dest);

#endif