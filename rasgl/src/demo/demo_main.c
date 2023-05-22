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

    if (input_state->keys[RAS_KEY_Q] == 1) {
        viewer_angle = viewer_angle - ROTATION_SPEED;
        if (viewer_angle <= 0) {
            viewer_angle += 359;
        }
        printf("a: %d\n", viewer_angle);
    }
    if (input_state->keys[RAS_KEY_E] == 1) {
        viewer_angle = viewer_angle + ROTATION_SPEED;
        if (viewer_angle > 359) {
            viewer_angle -= 359;
        }
        printf("a: %d\n", viewer_angle);
    }

    Point2f origin = { .x = 0, .y = 0 };
    Point2f vector;
    apply_unit_vector(&origin, viewer_angle, &vector);

    // translate direction vector to world space
    Point2f world_vector = { viewer_pos.x + vector.x, viewer_pos.y + vector.y };

    Point2f delta = {
        mul_fixed_16_16_by_fixed_16_16(viewer_pos.x - world_vector.x, VIEWER_SPEED),
        mul_fixed_16_16_by_fixed_16_16(viewer_pos.y - world_vector.y, VIEWER_SPEED)
    };

    if (input_state->keys[RAS_KEY_W] == 1) {
        printf("v: %d v: %d\n", vector.x, vector.y);
        printf("d: %d d: %d\n", delta.x, delta.y);
        viewer_pos.y += delta.y;
        viewer_pos.x += delta.x;
    }
    if (input_state->keys[RAS_KEY_A] == 1) {
        viewer_pos.x -= INT_32_TO_FIXED_16_16(1);
        printf("x: %d", viewer_pos.x);
    }
    if (input_state->keys[RAS_KEY_S] == 1) {
        viewer_pos.y -= INT_32_TO_FIXED_16_16(1);
    }
    if (input_state->keys[RAS_KEY_D] == 1) {
        viewer_pos.x += INT_32_TO_FIXED_16_16(1);
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

void render_map(RenderState* render_state)
{
    Point2f dest;
    Point2i* screen_pos;
    Point2f source;
    size_t* num_points = &render_state->num_points;
    size_t* num_commands = &render_state->num_commands;

    int32_t offset_x = MAP_COLS / -2;
    int32_t offset_y = MAP_ROWS / -2;

    for (int r = 0; r < MAP_ROWS; r++) {
        for (int c = 0; c < MAP_COLS; c++) {
            //  world space of map node
            source.x = (c + offset_x) * CELL_UNITS;
            source.y = (r + offset_y) * CELL_UNITS;
            render_point(render_state, source);
        }
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

    Point2f origin = { .x = 0, .y = 0 };
    Point2f vector;
    apply_unit_vector(&origin, viewer_angle, &vector);
    vector.x *= 4;
    vector.y *= 4;
    world_pos.x = viewer_pos.x + vector.x;
    world_pos.y = viewer_pos.y + vector.y;

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
