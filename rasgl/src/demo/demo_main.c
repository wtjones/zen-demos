#include "rasgl/core/app.h"
#include "rasgl/core/fixed_maths.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/input.h"
#include "rasgl/core/repr.h"
#include <stdio.h>

#define MAP_ROWS 5
#define MAP_COLS 5
#define MAX_WORLD_POINTS 1024
#define CELL_UNITS INT_32_TO_FIXED_16_16(10)
#define ZOOM_SPEED float_to_fixed_16_16(.05)
#define ROTATION_SPEED 4
#define VIEWER_SPEED float_to_fixed_16_16(.5)

typedef struct WorldState {
    Point3f points[MAX_WORLD_POINTS];
    size_t num_points;
} WorldState;

enum {
    MAP,
    CAMERA
} view_mode
    = CAMERA;

Point3f viewer_pos;
int32_t viewer_angle = 0;
ScreenSettings* settings;
WorldState world_state;

int map[MAP_COLS][MAP_ROWS] = {
    { 1, 1, 1, 1, 1 },
    { 1, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 1 },
    { 1, 1, 1, 1, 1 }
};

void push_world_point3(WorldState* world_state, Point3f point)
{
    size_t* num_points = &world_state->num_points;
    Point3f* dest = &world_state->points[*num_points];
    dest->x = point.x;
    dest->y = point.y;
    dest->z = point.z;
    (*num_points)++;
}

void map_to_world(WorldState* world_state)
{

    int32_t offset_x = MAP_COLS / -2;
    int32_t offset_y = 0;
    int32_t offset_z = MAP_ROWS / -2;
    size_t i = 0;
    for (int r = 0; r < MAP_ROWS + 1; r++) {
        for (int c = 0; c < MAP_COLS + 1; c++) {
            Point3f point;
            point.x = (c + offset_x) * CELL_UNITS;
            point.y = 0;
            point.z = -(r + offset_z) * CELL_UNITS;
            push_world_point3(world_state, point);
        }
    }
}

void xform_to_view(WorldState* world_state, RenderState* render_state, Point3f* viewer_pos)
{
    int32_t view_matrix[4][4];
    int32_t view_point[4];
    char buffer[255];
    mat_set_identity_4x4(view_matrix);

    int32_t sw = INT_32_TO_FIXED_16_16(settings->screen_width);
    int32_t sh = INT_32_TO_FIXED_16_16(settings->screen_height);
    int32_t offset_x = (sw / (int32_t)2) - viewer_pos->x;
    int32_t offset_y = (sh / (int32_t)2) - -viewer_pos->y;

    view_matrix[2][3] = INT_32_TO_FIXED_16_16(-10);
    view_matrix[0][3] = offset_x;
    view_matrix[1][3] = offset_y;

    size_t* num_points = &render_state->num_points;
    size_t* num_commands = &render_state->num_commands;

    for (int i = 0; i < world_state->num_points; i++) {
        Point3f* world_point = &world_state->points[i];

        int32_t v[4] = {
            world_point->x,
            world_point->y,
            world_point->z,
            INT_32_TO_FIXED_16_16(1)
        };

        // printf("vew trans matrix: %s\n", repr_mat_4x4(buffer, sizeof buffer, view_matrix));
        // printf("vew trans before: %s\n", repr_mat_4x1(buffer, sizeof buffer, v));
        mat_mul_4x4_4x1(view_matrix, v, view_point);
        // printf("vew trans: %s\n", repr_mat_4x1(buffer, sizeof buffer, view_point));

        RenderCommand* command = &render_state->commands[*num_commands];
        Point3f* screen_point = &render_state->points[*num_points];
        screen_point->x = FIXED_16_16_TO_INT_32(view_point[0]);
        screen_point->y = FIXED_16_16_TO_INT_32(view_point[1]);
        command->num_points = 1;
        command->point_indices[0] = *num_points;
        (*num_commands)++;
        (*num_points)++;
    }
}

void repr_world(char* buffer, size_t count, WorldState* world_state)
{
    for (int r = 0; r < MAP_ROWS + 1; r++) {
        for (int c = 0; c < MAP_COLS + 1; c++) {
        }
    }
}

void ras_app_init(int argc, const char** argv, ScreenSettings* init_settings)
{
    printf("ras_app_init()... screen_width.x: %d\n", init_settings->screen_width);
    settings = init_settings;

    viewer_pos.x = INT_32_TO_FIXED_16_16(0);
    viewer_pos.y = -INT_32_TO_FIXED_16_16(2);
    viewer_pos.z = INT_32_TO_FIXED_16_16(0);

    map_to_world(&world_state);
}

void ras_app_update(InputState* input_state)
{

    if (input_state->keys[RAS_KEY_Q] == 1 | input_state->keys[RAS_KEY_LEFT]) {
        viewer_angle = viewer_angle - ROTATION_SPEED;
        if (viewer_angle <= 0) {
            viewer_angle += 359;
        }
    }
    if (input_state->keys[RAS_KEY_E] == 1 | input_state->keys[RAS_KEY_RIGHT]) {
        viewer_angle = viewer_angle + ROTATION_SPEED;
        if (viewer_angle > 359) {
            viewer_angle -= 359;
        }
    }

    Point2f origin = { .x = 0, .y = 0 };
    Point2f vector;
    apply_unit_vector(&origin, viewer_angle, &vector);

    // translate direction vector to world space
    Point2f world_vector = { viewer_pos.x + vector.x, viewer_pos.z + vector.y };

    Point2f delta = {
        mul_fixed_16_16_by_fixed_16_16(world_vector.x - viewer_pos.x, VIEWER_SPEED),
        mul_fixed_16_16_by_fixed_16_16(world_vector.y - viewer_pos.z, VIEWER_SPEED)
    };

    Point3f viewer_pos_prev;
    memcpy(&viewer_pos_prev, &viewer_pos, sizeof viewer_pos);

    if (input_state->keys[RAS_KEY_W] == 1) {
        viewer_pos.z += delta.y;
        viewer_pos.x += delta.x;
    }
    if (input_state->keys[RAS_KEY_A] == 1) {
        viewer_pos.z -= delta.y;
        viewer_pos.x += delta.x;
    }
    if (input_state->keys[RAS_KEY_S] == 1) {
        viewer_pos.z -= delta.y;
        viewer_pos.x -= delta.x;
    }
    if (input_state->keys[RAS_KEY_D] == 1) {
        viewer_pos.z += delta.y;
        viewer_pos.x -= delta.x;
    }
    if (input_state->keys[RAS_KEY_EQUALS] == 1) {
        viewer_pos.y += ZOOM_SPEED;
    }
    if (input_state->keys[RAS_KEY_MINUS] == 1) {
        viewer_pos.y -= ZOOM_SPEED;
    }
    if (input_state->keys[RAS_KEY_TAB] == 1) {
        view_mode = view_mode == CAMERA ? MAP : CAMERA;
    }

    if (!cmp_point3f(&viewer_pos, &viewer_pos_prev)) {
        char buffer[100];
        printf("viewer_pos: %s\n", repr_point3f(buffer, sizeof buffer, &viewer_pos));
    }
}

/**
 * Translate a world position to screen and add to render commands.
 */
void render_point(RenderState* render_state, Point2f world_pos)
{
    size_t* num_points = &render_state->num_points;
    size_t* num_commands = &render_state->num_commands;
    Point2i* screen_pos;
    Point2f dest;

    screen_pos = &render_state->points[*num_points];
    xform_to_screen(
        settings->screen_width,
        settings->screen_height,
        &viewer_pos, &world_pos, &dest);

    screen_pos->x = FIXED_16_16_TO_INT_32(dest.x);
    screen_pos->y = FIXED_16_16_TO_INT_32(dest.y);

    render_state->commands[*num_commands].num_points = 1;
    render_state->commands[*num_commands].point_indices[0] = *num_points;

    (*num_points)++;
    (*num_commands)++;
}

/**
 * Transform world point to screen and add to render list
 */
void push_world_point(RenderState* render_state, Point2f world_pos)
{
    size_t* num_points = &render_state->num_points;
    size_t* num_commands = &render_state->num_commands;
    Point2i* screen_pos;
    Point2f dest;

    screen_pos = &render_state->points[*num_points];
    xform_to_screen(
        settings->screen_width,
        settings->screen_height,
        &viewer_pos, &world_pos, &dest);

    screen_pos->x = FIXED_16_16_TO_INT_32(dest.x);
    screen_pos->y = FIXED_16_16_TO_INT_32(dest.y);
    (*num_points)++;
}

void render_map(RenderState* render_state)
{
    Point2f dest;
    Point2i* screen_pos;
    Point2f source;
    size_t* num_points = &render_state->num_points;
    size_t* num_commands = &render_state->num_commands;

    int32_t offset_x = MAP_COLS / -2;
    int32_t offset_y = MAP_ROWS / -2;

    // Add map points to render list
    size_t map_points_start = render_state->num_points;
    for (int r = 0; r < MAP_ROWS + 1; r++) {
        for (int c = 0; c < MAP_COLS + 1; c++) {
            // world space of map node
            source.x = (c + offset_x) * CELL_UNITS;
            source.y = -(r + offset_y) * CELL_UNITS;
            push_world_point(render_state, source);
        }
    }

    // Each cell has 4 points. Up to 4 points are shared
    // with neighbors.
    // consider local point of each cell:
    // cp0--cp1
    // |    |
    // cp2--cp3

    RenderCommand* command = &render_state->commands[*num_commands];

    // initalize the indices of the 4 cell points
    size_t cp0 = map_points_start;
    size_t cp1 = cp0 + 1;
    size_t cp2 = map_points_start + MAP_COLS + 1; // next row
    size_t cp3 = cp2 + 1;

    // Construct triangles and add as commands
    for (int r = 0; r < MAP_ROWS; r++) {
        for (int c = 0; c < MAP_COLS; c++) {

            command->num_points = 3;
            command->point_indices[0] = cp0;
            command->point_indices[1] = cp1;
            command->point_indices[2] = cp2;
            (*num_commands)++;
            command = &render_state->commands[*num_commands];

            command->num_points = 3;
            command->point_indices[0] = cp1;
            command->point_indices[1] = cp3;
            command->point_indices[2] = cp2;
            (*num_commands)++;
            command = &render_state->commands[*num_commands];

            cp0++;
            cp1++;
            cp2++;
            cp3++;
        }
        cp0++;
        cp1++;
        cp2++;
        cp3++;
    }
}

void render_origin(RenderState* render_state)
{
    Point2f world_pos;

    world_pos.x = INT_32_TO_FIXED_16_16(0);
    world_pos.y = INT_32_TO_FIXED_16_16(1);
    render_point(render_state, world_pos);
    world_pos.x = INT_32_TO_FIXED_16_16(0);
    world_pos.y = INT_32_TO_FIXED_16_16(2);
    render_point(render_state, world_pos);
    world_pos.x = INT_32_TO_FIXED_16_16(0);
    world_pos.y = INT_32_TO_FIXED_16_16(3);
    render_point(render_state, world_pos);
    world_pos.x = INT_32_TO_FIXED_16_16(0);
    world_pos.y = INT_32_TO_FIXED_16_16(4);
    render_point(render_state, world_pos);

    world_pos.x = INT_32_TO_FIXED_16_16(1);
    world_pos.y = INT_32_TO_FIXED_16_16(0);
    render_point(render_state, world_pos);
    world_pos.x = INT_32_TO_FIXED_16_16(0);
    world_pos.y = -INT_32_TO_FIXED_16_16(1);
    render_point(render_state, world_pos);
    world_pos.x = -INT_32_TO_FIXED_16_16(1);
    world_pos.y = INT_32_TO_FIXED_16_16(0);
    render_point(render_state, world_pos);
}

void render_viewer(RenderState* render_state)
{
    Point2f world_pos;

    world_pos.x = viewer_pos.x + INT_32_TO_FIXED_16_16(0);
    world_pos.y = viewer_pos.z + INT_32_TO_FIXED_16_16(1);
    render_point(render_state, world_pos);

    world_pos.x = viewer_pos.x + INT_32_TO_FIXED_16_16(1);
    world_pos.y = viewer_pos.z + INT_32_TO_FIXED_16_16(0);
    render_point(render_state, world_pos);

    world_pos.x = viewer_pos.x + INT_32_TO_FIXED_16_16(0);
    world_pos.y = viewer_pos.z + -INT_32_TO_FIXED_16_16(1);
    render_point(render_state, world_pos);

    world_pos.x = viewer_pos.x + -INT_32_TO_FIXED_16_16(1);
    world_pos.y = viewer_pos.z + INT_32_TO_FIXED_16_16(0);
    render_point(render_state, world_pos);

    // render vector
    Point2f origin = { .x = 0, .y = 0 };
    Point2f unit_vector;
    apply_unit_vector(&origin, viewer_angle, &unit_vector);

    world_pos.x = viewer_pos.x + (unit_vector.x * 8);
    world_pos.y = viewer_pos.z + (unit_vector.y * 8);
    render_point(render_state, world_pos);

    world_pos.x = viewer_pos.x + -(unit_vector.y * 4);
    world_pos.y = viewer_pos.z + (unit_vector.x * 4);
    render_point(render_state, world_pos);

    world_pos.x = viewer_pos.x + (unit_vector.y * 4);
    world_pos.y = viewer_pos.z + -(unit_vector.x * 4);
    render_point(render_state, world_pos);
}

void ras_app_render(RenderState* render_state)
{
    render_state->num_points = 0;
    render_state->num_commands = 0;
    if (view_mode == MAP) {
        render_map(render_state);
        render_origin(render_state);
        render_viewer(render_state);
    } else {
        xform_to_view(&world_state, render_state, &viewer_pos);
    }
}
