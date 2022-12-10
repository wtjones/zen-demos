#include "maths.h"

float cos_table[3600];
float sin_table[3600];

void init_math_lookups()
{

    for (int i = 0; i < 3600; i++) {
        cos_table[i] = cos((float)i * M_PI / -1800.0);
        sin_table[i] = sin((float)i * M_PI / -1800.0);
    }
}

void apply_unit_vector(Point2f* src, int angle, Point2f* dest)
{
    float xform_result[3];
    float c = cos_table[angle * 10];
    float s = sin_table[angle * 10];

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

void xform_to_world(
    Point2f* position,
    float angle_cos,
    float angle_sin,
    Point2f* source,
    Point2f* dest)
{
    // Rotate and translate to world coord
    float matrix[3][3] = {
        { angle_cos, -angle_sin, position->x },
        { angle_sin, angle_cos, position->y },
        { 0, 0, 1.0 }
    };

    float pos[3] = { source->x, source->y, 1.0 };
    float xform_result[3];
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
    float offset_x = (screen_width / 2) - viewer_pos->x;
    float offset_y = (screen_height / 2) - viewer_pos->y;

    // Scale and translate to screen coord
    float scale_matrix[3][3] = {
        { viewer_pos->z, 0.0, offset_x },
        { 0.0, viewer_pos->z, offset_y },
        { 0.0, 0.0, 1.0 }
    };
    float scale_result[3];
    float pos[3] = { source->x, source->y, 1.0 };
    mat_mul_3x3_3x1(scale_matrix, pos, scale_result);

    dest->x = scale_result[0];
    dest->y = scale_result[1];
}
