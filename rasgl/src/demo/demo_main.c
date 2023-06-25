#include "rasgl/core/app.h"
#include "rasgl/core/fixed_maths.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/input.h"
#include <stdio.h>

#define MAP_ROWS 5
#define MAP_COLS 5
#define CELL_UNITS INT_32_TO_FIXED_16_16(10)
#define ZOOM_SPEED float_to_fixed_16_16(.05)
#define ROTATION_SPEED 4
#define VIEWER_SPEED float_to_fixed_16_16(.5)

Point3f viewer_pos;
int32_t viewer_angle = 0;
ScreenSettings* settings;

int map[MAP_COLS][MAP_ROWS] = {
    { 1, 1, 1, 1, 1 },
    { 1, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 1 },
    { 1, 1, 1, 1, 1 }
};

void ras_app_init(int argc, const char** argv, ScreenSettings* init_settings)
{
    printf("ras_app_init()... screen_width.x: %d\n", init_settings->screen_width);
    settings = init_settings;

    viewer_pos.x = INT_32_TO_FIXED_16_16(0);
    viewer_pos.y = INT_32_TO_FIXED_16_16(0);
    viewer_pos.z = INT_32_TO_FIXED_16_16(1);
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
    Point2f world_vector = { viewer_pos.x + vector.x, viewer_pos.y + vector.y };

    Point2f delta = {
        mul_fixed_16_16_by_fixed_16_16(world_vector.x - viewer_pos.x, VIEWER_SPEED),
        mul_fixed_16_16_by_fixed_16_16(world_vector.y - viewer_pos.y, VIEWER_SPEED)
    };

    if (input_state->keys[RAS_KEY_W] == 1) {
        viewer_pos.y += delta.y;
        viewer_pos.x += delta.x;
    }
    if (input_state->keys[RAS_KEY_A] == 1) {
        viewer_pos.x -= delta.y;
        viewer_pos.y += delta.x;
    }
    if (input_state->keys[RAS_KEY_S] == 1) {
        viewer_pos.y -= delta.y;
        viewer_pos.x -= delta.x;
    }
    if (input_state->keys[RAS_KEY_D] == 1) {
        viewer_pos.x += delta.y;
        viewer_pos.y -= delta.x;
    }
    if (input_state->keys[RAS_KEY_EQUALS] == 1) {
        viewer_pos.z += ZOOM_SPEED;
    }
    if (input_state->keys[RAS_KEY_MINUS] == 1) {
        viewer_pos.z -= ZOOM_SPEED;
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
    world_pos.y = viewer_pos.y + INT_32_TO_FIXED_16_16(1);
    render_point(render_state, world_pos);

    world_pos.x = viewer_pos.x + INT_32_TO_FIXED_16_16(1);
    world_pos.y = viewer_pos.y + INT_32_TO_FIXED_16_16(0);
    render_point(render_state, world_pos);

    world_pos.x = viewer_pos.x + INT_32_TO_FIXED_16_16(0);
    world_pos.y = viewer_pos.y + -INT_32_TO_FIXED_16_16(1);
    render_point(render_state, world_pos);

    world_pos.x = viewer_pos.x + -INT_32_TO_FIXED_16_16(1);
    world_pos.y = viewer_pos.y + INT_32_TO_FIXED_16_16(0);
    render_point(render_state, world_pos);

    // render vector
    Point2f origin = { .x = 0, .y = 0 };
    Point2f unit_vector;
    apply_unit_vector(&origin, viewer_angle, &unit_vector);

    world_pos.x = viewer_pos.x + (unit_vector.x * 8);
    world_pos.y = viewer_pos.y + (unit_vector.y * 8);
    render_point(render_state, world_pos);

    world_pos.x = viewer_pos.x + -(unit_vector.y * 4);
    world_pos.y = viewer_pos.y + (unit_vector.x * 4);
    render_point(render_state, world_pos);

    world_pos.x = viewer_pos.x + (unit_vector.y * 4);
    world_pos.y = viewer_pos.y + -(unit_vector.x * 4);
    render_point(render_state, world_pos);
}

void ras_app_render(RenderState* render_state)
{
    render_state->num_points = 0;
    render_state->num_commands = 0;
    render_map(render_state);
    render_origin(render_state);
    render_viewer(render_state);
}
