#include "maths.h"

float cos_table[360];
float sin_table[360];

void init_math_lookups()
{

    for (int i = 0; i < 360; i++) {
        cos_table[i] = cos((float)i * M_PI / -180.0);
        sin_table[i] = sin((float)i * M_PI / -180.0);
    }
}

void apply_unit_vector(Point2f* src, int angle, Point2f* dest)
{
    float xform_result[3];
    float c = cos_table[angle];
    float s = sin_table[angle];

    float matrix[3][3] = {
        { c, -s, src->x },
        { s, c, src->y },
        { 0, 0, 1.0 }
    };
    float pos[3] = { 0.0, 1.0, 1.0 };
    mat_mul_3x3_3x1(matrix, pos, xform_result);

    dest->x = xform_result[0];
    dest->y = xform_result[1];
}
