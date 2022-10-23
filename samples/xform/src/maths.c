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

float cross(Point2f* v1, Point2f* v2)
{
    return v1->x * v2->y - v1->y * v2->x;
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

        float facing = cross(&v1, &v2);

        if (facing < 0.0) {
            collide_p1->x = side_p1->x;
            collide_p1->y = side_p1->y;
            collide_p2->x = side_p2->x;
            collide_p2->y = side_p2->y;

            return true;
        }
    }
    return false;
}