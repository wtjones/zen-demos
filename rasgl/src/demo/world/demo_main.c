#include "rasgl/core/app.h"
#include "rasgl/core/camera.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/fixed_maths.h"
#include "rasgl/core/frustum.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/input.h"
#include "rasgl/core/repr.h"
#include <stdio.h>
#include <stdlib.h>

#define MAP_ROWS 5
#define MAP_COLS 5
#define MAX_WORLD_POINTS 1024
#define CELL_UNITS INT_32_TO_FIXED_16_16(10)

typedef struct WorldState {
    Point3f points[MAX_WORLD_POINTS];
    uint32_t num_points;
} WorldState;

enum {
    MAP,
    CAMERA
} view_mode
    = CAMERA;

RasCamera camera = {
    .angle = 180,
    .aspect_ratio = 1.333f,
    .near = 0.1f,
    .far = 100.0f,
    .fov = 60.0f,
    .projection_mode = RAS_PERSPECTIVE_MATRIX,
    .last_changed_frame = 0
};

bool viewer_changed = true;

ScreenSettings* settings;
WorldState world_state;
RasFrustum frustum;

int map[MAP_ROWS][MAP_COLS] = {
    { 1, 1, 0, 1, 1 },
    { 1, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 0 },
    { 1, 0, 0, 0, 1 },
    { 1, 1, 1, 1, 1 }
};

void push_world_point3(WorldState* world_state, Point3f point)
{
    uint32_t* num_points = &world_state->num_points;
    Point3f* dest = &world_state->points[*num_points];
    dest->x = point.x;
    dest->y = point.y;
    dest->z = point.z;
    (*num_points)++;
}

void map_to_world(WorldState* world_state)
{

    int32_t offset_x = -(MAP_COLS / 2);
    int32_t offset_z = -(MAP_ROWS / 2);

    for (int r = 0; r < MAP_ROWS; r++) {
        for (int c = 0; c < MAP_COLS; c++) {
            if (map[r][c] == 1) {
                Point3f point;
                point.x = (c + offset_x) * CELL_UNITS;
                point.y = -INT_32_TO_FIXED_16_16(10);
                point.z = (r + offset_z) * CELL_UNITS;

                push_world_point3(world_state, point);
            }
        }
    }
}

void origin_to_world(WorldState* world_state)
{
    Point3f point;

    point.x = INT_32_TO_FIXED_16_16(2);
    point.y = INT_32_TO_FIXED_16_16(0);
    point.z = INT_32_TO_FIXED_16_16(0);
    push_world_point3(world_state, point);

    point.x = -INT_32_TO_FIXED_16_16(2);
    point.y = INT_32_TO_FIXED_16_16(0);
    point.z = INT_32_TO_FIXED_16_16(0);
    push_world_point3(world_state, point);

    point.x = INT_32_TO_FIXED_16_16(0);
    point.y = INT_32_TO_FIXED_16_16(2);
    point.z = INT_32_TO_FIXED_16_16(0);
    push_world_point3(world_state, point);

    point.x = INT_32_TO_FIXED_16_16(0);
    point.y = -INT_32_TO_FIXED_16_16(2);
    point.z = INT_32_TO_FIXED_16_16(0);
    push_world_point3(world_state, point);

    point.x = INT_32_TO_FIXED_16_16(0);
    point.y = INT_32_TO_FIXED_16_16(0);
    point.z = INT_32_TO_FIXED_16_16(2);
    push_world_point3(world_state, point);

    point.x = INT_32_TO_FIXED_16_16(0);
    point.y = INT_32_TO_FIXED_16_16(0);
    point.z = -INT_32_TO_FIXED_16_16(2);
    push_world_point3(world_state, point);
}

void xform_to_view_mode_persp_matrix(
    WorldState* world_state, RenderState* render_state, RasCamera* camera)
{
    // int32_t translate_to_viewer[4][4];
    int32_t view_matrix[4][4];
    int32_t view_point[4];
    int32_t projected_point[4];

    char buffer[1000];

    ras_camera_world_view_init(camera, view_matrix);
    int32_t projection_matrix[4][4];
    int32_t combined_matrix[4][4];

    ras_camera_projection_init(camera, projection_matrix);
    mat_mul_4x4_4x4(projection_matrix, view_matrix, combined_matrix);

    core_frustum_init(combined_matrix, &frustum);

    if (camera->last_changed_frame == render_state->current_frame) {
        ras_log_info("view proj matrix: %s\n", repr_mat_4x4(buffer, sizeof buffer, combined_matrix));
        ras_log_info("frustum: %s\n", core_repr_frustum(buffer, sizeof buffer, &frustum));
    }
    uint32_t* num_points = &render_state->num_points;
    uint32_t* num_commands = &render_state->num_commands;

    for (uint32_t i = 0; i < world_state->num_points; i++) {
        Point3f* world_point = &world_state->points[i];

        int32_t world_vec[4] = {
            world_point->x,
            world_point->y,
            world_point->z,
            INT_32_TO_FIXED_16_16(1)
        };

        ras_log_trace("view trans before: %s\n", repr_mat_4x1(buffer, sizeof buffer, world_vec));

        mat_mul_4x4_4x1(view_matrix, world_vec, view_point);
        ras_log_trace("rot trans after: %s\n", repr_mat_4x1(buffer, sizeof buffer, view_point));

        if (view_point[2] < 0) {
            ras_log_trace("proj matrix: %s\n", repr_mat_4x4(buffer, sizeof buffer, projection_matrix));

            mat_mul_project(projection_matrix, view_point, projected_point);
            ras_log_trace("proj multi after: %s\n", repr_mat_4x1(buffer, sizeof buffer, projected_point));

            RenderCommand* command = &render_state->commands[*num_commands];
            Point2i* screen_point = &render_state->points[*num_points];

            projected_to_screen_point(settings->screen_width, settings->screen_height, projected_point, screen_point);

            ras_log_trace("screen point: %s\n", repr_point2i(buffer, sizeof buffer, screen_point));

            command->num_points = 1;
            command->point_indices[0] = *num_points;
            (*num_commands)++;
            (*num_points)++;
        }
    }
}

void xform_to_view(WorldState* world_state, RenderState* render_state, RasCamera* camera)
{
    xform_to_view_mode_persp_matrix(world_state, render_state, camera);
}

RasResult ras_app_init(int argc, const char** argv, ScreenSettings* init_settings)
{
    ras_log_info("ras_app_init()... argc: %d argv: %s\n", argc, argv[0]);
    ras_log_info("ras_app_init()... screen_width.x: %d\n", init_settings->screen_width);
    settings = init_settings;

    camera.position.x = INT_32_TO_FIXED_16_16(0);
    camera.position.y = INT_32_TO_FIXED_16_16(2);
    camera.position.z = INT_32_TO_FIXED_16_16(40);

    map_to_world(&world_state);
    origin_to_world(&world_state);
    return RAS_RESULT_OK;
}

void ras_app_update(InputState* input_state)
{
    ras_camera_update(&camera, input_state);

    if (input_state->keys[RAS_KEY_TAB] == RAS_KEY_EVENT_UP) {
        view_mode = view_mode == CAMERA ? MAP : CAMERA;
    }
}

/**
 * Translate a world position to screen and add to render commands.
 */
void render_point(RenderState* render_state, Point3f world_pos)
{
    uint32_t* num_points = &render_state->num_points;
    uint32_t* num_commands = &render_state->num_commands;
    Point2i* screen_pos;
    Point2f dest;

    screen_pos = &render_state->points[*num_points];
    xform_to_screen(
        settings->screen_width,
        settings->screen_height,
        &camera.position, &world_pos, &dest);

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
void push_world_point(RenderState* render_state, Point3f world_pos)
{
    uint32_t* num_points = &render_state->num_points;
    Point2i* screen_pos;
    Point2f dest;

    screen_pos = &render_state->points[*num_points];
    xform_to_screen(
        settings->screen_width,
        settings->screen_height,
        &camera.position, &world_pos, &dest);

    screen_pos->x = FIXED_16_16_TO_INT_32(dest.x);
    screen_pos->y = FIXED_16_16_TO_INT_32(dest.y);
    (*num_points)++;
}

void render_map(RenderState* render_state)
{
    Point3f source;
    uint32_t* num_commands = &render_state->num_commands;

    int32_t offset_x = -(MAP_COLS / 2);
    int32_t offset_z = -(MAP_ROWS / 2);

    // Add map points to render list
    uint32_t map_points_start = render_state->num_points;
    for (int r = 0; r < MAP_ROWS + 1; r++) {
        for (int c = 0; c < MAP_COLS + 1; c++) {
            // world space of map node
            source.x = (c + offset_x) * CELL_UNITS;
            source.y = 0;
            source.z = (r + offset_z) * CELL_UNITS;
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

            if (map[r][c] == 1) {
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
            }

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

void render_frustum(RenderState* render_state)
{
    Point3f source;
    uint32_t* num_commands = &render_state->num_commands;
    RenderCommand* command = &render_state->commands[*num_commands];
    uint32_t frustum_points_start = render_state->num_points;

    // add points
    source.x = frustum.points[0].x;
    source.y = 0;
    source.z = frustum.points[0].z;
    push_world_point(render_state, source);

    source.x = frustum.points[1].x;
    source.y = 0;
    source.z = frustum.points[1].z;
    push_world_point(render_state, source);

    source.x = frustum.points[2].x;
    source.y = 0;
    source.z = frustum.points[2].z;
    push_world_point(render_state, source);

    source.x = frustum.points[3].x;
    source.y = 0;
    source.z = frustum.points[3].z;
    push_world_point(render_state, source);

    // left side
    command->num_points = 2;
    command->point_indices[0] = frustum_points_start;
    command->point_indices[1] = frustum_points_start + 1;
    (*num_commands)++;
    command = &render_state->commands[*num_commands];

    // near side
    command->num_points = 2;
    command->point_indices[0] = frustum_points_start + 1;
    command->point_indices[1] = frustum_points_start + 2;
    (*num_commands)++;
    command = &render_state->commands[*num_commands];

    // right side
    command->num_points = 2;
    command->point_indices[0] = frustum_points_start + 2;
    command->point_indices[1] = frustum_points_start + 3;
    (*num_commands)++;
    command = &render_state->commands[*num_commands];

    // far side
    command->num_points = 2;
    command->point_indices[0] = frustum_points_start;
    command->point_indices[1] = frustum_points_start + 3;
    (*num_commands)++;
    command = &render_state->commands[*num_commands];

    // top-left point
    command->num_points = 1;
    command->point_indices[0] = frustum_points_start;
    (*num_commands)++;
    command = &render_state->commands[*num_commands];

    // near-left point
    command->num_points = 1;
    command->point_indices[0] = frustum_points_start + 1;
    (*num_commands)++;
    command = &render_state->commands[*num_commands];

    // near-right point
    command->num_points = 1;
    command->point_indices[0] = frustum_points_start + 2;
    (*num_commands)++;
    command = &render_state->commands[*num_commands];

    // far-right point
    command->num_points = 1;
    command->point_indices[0] = frustum_points_start + 3;
    (*num_commands)++;
    command = &render_state->commands[*num_commands];
}

void render_origin(RenderState* render_state)
{
    Point3f world_pos;

    world_pos.x = INT_32_TO_FIXED_16_16(0);
    world_pos.z = -INT_32_TO_FIXED_16_16(1);
    render_point(render_state, world_pos);
    world_pos.x = INT_32_TO_FIXED_16_16(0);
    world_pos.z = -INT_32_TO_FIXED_16_16(2);
    render_point(render_state, world_pos);
    world_pos.x = INT_32_TO_FIXED_16_16(0);
    world_pos.z = -INT_32_TO_FIXED_16_16(3);
    render_point(render_state, world_pos);
    world_pos.x = INT_32_TO_FIXED_16_16(0);
    world_pos.z = -INT_32_TO_FIXED_16_16(4);
    render_point(render_state, world_pos);

    world_pos.x = INT_32_TO_FIXED_16_16(1);
    world_pos.z = INT_32_TO_FIXED_16_16(0);
    render_point(render_state, world_pos);
    world_pos.x = INT_32_TO_FIXED_16_16(0);
    world_pos.z = INT_32_TO_FIXED_16_16(1);
    render_point(render_state, world_pos);
    world_pos.x = -INT_32_TO_FIXED_16_16(1);
    world_pos.z = INT_32_TO_FIXED_16_16(0);
    render_point(render_state, world_pos);
}

void render_viewer(RenderState* render_state)
{
    Point3f world_pos;

    world_pos.x = camera.position.x + INT_32_TO_FIXED_16_16(0);
    world_pos.z = camera.position.z + INT_32_TO_FIXED_16_16(1);
    render_point(render_state, world_pos);

    world_pos.x = camera.position.x + INT_32_TO_FIXED_16_16(1);
    world_pos.z = camera.position.z + INT_32_TO_FIXED_16_16(0);
    render_point(render_state, world_pos);

    world_pos.x = camera.position.x + INT_32_TO_FIXED_16_16(0);
    world_pos.z = camera.position.z + -INT_32_TO_FIXED_16_16(1);
    render_point(render_state, world_pos);

    world_pos.x = camera.position.x + -INT_32_TO_FIXED_16_16(1);
    world_pos.z = camera.position.z + INT_32_TO_FIXED_16_16(0);
    render_point(render_state, world_pos);

    // render vector
    Point2f origin = { .x = 0, .y = 0 };
    Point2f unit_vector;
    apply_unit_vector(&origin, camera.angle, &unit_vector);

    world_pos.x = camera.position.x + (unit_vector.x * 8);
    world_pos.z = camera.position.z + (unit_vector.y * 8);

    render_point(render_state, world_pos);

    world_pos.x = camera.position.x + -(unit_vector.y * 4);
    world_pos.z = camera.position.z + (unit_vector.x * 4);
    render_point(render_state, world_pos);

    world_pos.x = camera.position.x + (unit_vector.y * 4);
    world_pos.z = camera.position.z + -(unit_vector.x * 4);
    render_point(render_state, world_pos);
}

void ras_app_render(RenderState* render_state)
{
    render_state->num_points = 0;
    render_state->num_commands = 0;
    if (view_mode == MAP) {
        xform_to_view(&world_state, render_state, &camera);
        render_map(render_state);
        render_frustum(render_state);
        render_origin(render_state);
        render_viewer(render_state);
    } else {
        xform_to_view(&world_state, render_state, &camera);
    }
}
