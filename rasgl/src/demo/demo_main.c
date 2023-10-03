#include "rasgl/core/app.h"
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
#define ZOOM_SPEED float_to_fixed_16_16(.05)
#define ROTATION_SPEED 1
#define FOV_SPEED 2.0f
#define VIEWER_SPEED float_to_fixed_16_16(.5)
#define PROJECTION_RATIO -float_to_fixed_16_16(2.0)

typedef struct WorldState {
    Point3f points[MAX_WORLD_POINTS];
    uint32_t num_points;
} WorldState;

enum {
    MAP,
    CAMERA
} view_mode
    = CAMERA;

enum {
    PERSPECTIVE_MATRIX,
    PERSPECTIVE_ALT
} projection_mode
    = PERSPECTIVE_MATRIX;

Point3f viewer_pos;
int32_t viewer_angle = 180;
float viewer_fov = 60.0f;

ScreenSettings* settings;
WorldState world_state;
Frustum frustum;

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
    WorldState* world_state, RenderState* render_state, Point3f* viewer_pos)
{
    int32_t translate_to_viewer[4][4];
    int32_t view_matrix[4][4];
    int32_t view_point[4];
    int32_t projected_point[4];

    char buffer[255];

    int32_t angle = (viewer_angle + 180) % 360;
    if (angle < 0) {
        angle += 360;
    }

    // Combine world to viewer translate and rotate operations
    Point3f trans_pos = { -viewer_pos->x, -viewer_pos->y, -viewer_pos->z };
    mat_translate_init(translate_to_viewer, &trans_pos);
    mat_rotate_y(translate_to_viewer, angle, view_matrix);

    int32_t projection_matrix[4][4];
    int32_t combined_matrix[4][4];

    float aspect_ratio = 1.333f; // Aspect ratio (width/height)
    float near = 0.1f;           // Near clipping plane
    float far = 100.0f;          // Far clipping plane

    mat_projection_init(projection_matrix, viewer_fov, aspect_ratio, near, far);

    mat_mul_4x4_4x4(projection_matrix, view_matrix, combined_matrix);
    ras_log_info("view proj matrix: %s\n", repr_mat_4x4(buffer, sizeof buffer, combined_matrix));

    core_frustum_init(combined_matrix, &frustum);

    ras_log_trace("frustum left plane: %s\n", repr_point3f(buffer, sizeof buffer, &frustum.planes[PLANE_LEFT].normal));
    ras_log_trace("frustum left plane dist: %s\n", repr_fixed_16_16(buffer, sizeof buffer, frustum.planes[PLANE_LEFT].distance));

    ras_log_trace("frustum right plane: %s\n", repr_point3f(buffer, sizeof buffer, &frustum.planes[PLANE_RIGHT].normal));
    ras_log_trace("frustum right plane dist: %s\n", repr_fixed_16_16(buffer, sizeof buffer, frustum.planes[PLANE_RIGHT].distance));

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

            int32_t half_screen_width = INT_32_TO_FIXED_16_16(settings->screen_width / 2);
            int32_t half_screen_height = INT_32_TO_FIXED_16_16(settings->screen_height / 2);

            screen_point->x = FIXED_16_16_TO_INT_32(
                mul_fixed_16_16_by_fixed_16_16(half_screen_width, projected_point[0]) + half_screen_width);
            screen_point->y = FIXED_16_16_TO_INT_32(
                mul_fixed_16_16_by_fixed_16_16(half_screen_height, projected_point[1]) + half_screen_height);

            ras_log_trace("screen point: %s\n", repr_point2i(buffer, sizeof buffer, screen_point));

            command->num_points = 1;
            command->point_indices[0] = *num_points;
            (*num_commands)++;
            (*num_points)++;
        }
    }
}

void xform_to_view_mode_persp_alt(WorldState* world_state, RenderState* render_state, Point3f* viewer_pos)
{
    int32_t translate_to_viewer[4][4];
    int32_t view_matrix[4][4];
    int32_t view_point[4];
    char buffer[255];

    int32_t angle = (viewer_angle + 180) % 360;
    if (angle < 0) {
        angle += 360;
    }

    // Combine world to viewer translate and rotate operations
    Point3f trans_pos = { -viewer_pos->x, -viewer_pos->y, -viewer_pos->z };
    mat_translate_init(translate_to_viewer, &trans_pos);
    mat_rotate_y(translate_to_viewer, angle, view_matrix);

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

        Point3f transformed = {
            .x = view_point[0],
            .y = view_point[1],
            .z = view_point[2]
        };

        if (transformed.z < 0) {

            Point2f projected = project_point(
                settings->screen_width,
                settings->screen_height,
                PROJECTION_RATIO,
                transformed);

            RenderCommand* command = &render_state->commands[*num_commands];
            Point2i* screen_point = &render_state->points[*num_points];
            screen_point->x = FIXED_16_16_TO_INT_32(projected.x);
            screen_point->y = FIXED_16_16_TO_INT_32(projected.y);

            ras_log_trace("screen point: %s\n", repr_point2i(buffer, sizeof buffer, screen_point));

            command->num_points = 1;
            command->point_indices[0] = *num_points;
            (*num_commands)++;
            (*num_points)++;
        }
    }
}

void xform_to_view(WorldState* world_state, RenderState* render_state, Point3f* viewer_pos)
{
    if (projection_mode == PERSPECTIVE_MATRIX) {
        xform_to_view_mode_persp_matrix(world_state, render_state, viewer_pos);
    } else {
        xform_to_view_mode_persp_alt(world_state, render_state, viewer_pos);
    }
}

void ras_app_init(int argc, const char** argv, ScreenSettings* init_settings)
{
    ras_log_info("ras_app_init()... argc: %d argv: %s\n", argc, argv[0]);
    ras_log_info("ras_app_init()... screen_width.x: %d\n", init_settings->screen_width);
    settings = init_settings;

    viewer_pos.x = INT_32_TO_FIXED_16_16(0);
    viewer_pos.y = INT_32_TO_FIXED_16_16(2);
    viewer_pos.z = INT_32_TO_FIXED_16_16(40);

    map_to_world(&world_state);
    origin_to_world(&world_state);
}

void ras_app_update(InputState* input_state)
{
    int32_t viewer_angle_prev = viewer_angle;
    int32_t delta_angle = 0;
    if (input_state->keys[RAS_KEY_Q] == 1 || input_state->keys[RAS_KEY_LEFT]) {
        delta_angle = ROTATION_SPEED;
    }
    if (input_state->keys[RAS_KEY_E] == 1 || input_state->keys[RAS_KEY_RIGHT]) {
        delta_angle = -ROTATION_SPEED;
    }

    viewer_angle = (viewer_angle + delta_angle) % 360;
    if (viewer_angle < 0) {
        viewer_angle += 360;
    };

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
        viewer_pos.z -= delta.x;
        viewer_pos.x += delta.y;
    }
    if (input_state->keys[RAS_KEY_S] == 1) {
        viewer_pos.z -= delta.y;
        viewer_pos.x -= delta.x;
    }
    if (input_state->keys[RAS_KEY_D] == 1) {
        viewer_pos.z += delta.x;
        viewer_pos.x -= delta.y;
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
    if (input_state->keys[RAS_KEY_P] == 1) {
        projection_mode = projection_mode == PERSPECTIVE_MATRIX
            ? PERSPECTIVE_ALT
            : PERSPECTIVE_MATRIX;
    }
    if (input_state->keys[RAS_KEY_LEFTBRACKET] == 1) {
        viewer_fov -= FOV_SPEED;
        ras_log_info("FOV: %f\n", viewer_fov);
    }
    if (input_state->keys[RAS_KEY_RIGHTBRACKET] == 1) {
        viewer_fov += FOV_SPEED;
        ras_log_info("FOV: %f\n", viewer_fov);
    }

    if (!cmp_point3f(&viewer_pos, &viewer_pos_prev)) {
        char buffer[100];
        ras_log_info("viewer_pos: %s\n", repr_point3f(buffer, sizeof buffer, &viewer_pos));
    }
    if (viewer_angle != viewer_angle_prev) {
        ras_log_info("viewer_angle: %d\n", viewer_angle);
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
void push_world_point(RenderState* render_state, Point3f world_pos)
{
    uint32_t* num_points = &render_state->num_points;
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
    command->num_points = 1;
    command->point_indices[0] = frustum_points_start;
    (*num_commands)++;
    command = &render_state->commands[*num_commands];

    // near side
    command->num_points = 1;
    command->point_indices[0] = frustum_points_start + 1;
    (*num_commands)++;
    command = &render_state->commands[*num_commands];

    // right side
    command->num_points = 1;
    command->point_indices[0] = frustum_points_start + 2;
    (*num_commands)++;
    command = &render_state->commands[*num_commands];

    // far side
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

    world_pos.x = viewer_pos.x + INT_32_TO_FIXED_16_16(0);
    world_pos.z = viewer_pos.z + INT_32_TO_FIXED_16_16(1);
    render_point(render_state, world_pos);

    world_pos.x = viewer_pos.x + INT_32_TO_FIXED_16_16(1);
    world_pos.z = viewer_pos.z + INT_32_TO_FIXED_16_16(0);
    render_point(render_state, world_pos);

    world_pos.x = viewer_pos.x + INT_32_TO_FIXED_16_16(0);
    world_pos.z = viewer_pos.z + -INT_32_TO_FIXED_16_16(1);
    render_point(render_state, world_pos);

    world_pos.x = viewer_pos.x + -INT_32_TO_FIXED_16_16(1);
    world_pos.z = viewer_pos.z + INT_32_TO_FIXED_16_16(0);
    render_point(render_state, world_pos);

    // render vector
    Point2f origin = { .x = 0, .y = 0 };
    Point2f unit_vector;
    apply_unit_vector(&origin, viewer_angle, &unit_vector);

    world_pos.x = viewer_pos.x + (unit_vector.x * 8);
    world_pos.z = viewer_pos.z + (unit_vector.y * 8);

    if (render_state->current_frame % 30 == 0) {
        char buffer[100];
        char buffer2[100];
        ras_log_info("the tip: %s %s\n",
            repr_point3f(buffer, sizeof buffer, &world_pos),
            repr_point2f(buffer2, sizeof buffer2, &unit_vector));
    }
    render_point(render_state, world_pos);

    world_pos.x = viewer_pos.x + -(unit_vector.y * 4);
    world_pos.z = viewer_pos.z + (unit_vector.x * 4);
    render_point(render_state, world_pos);

    world_pos.x = viewer_pos.x + (unit_vector.y * 4);
    world_pos.z = viewer_pos.z + -(unit_vector.x * 4);
    render_point(render_state, world_pos);
}

void ras_app_render(RenderState* render_state)
{
    render_state->num_points = 0;
    render_state->num_commands = 0;
    if (view_mode == MAP) {
        xform_to_view(&world_state, render_state, &viewer_pos);
        render_map(render_state);
        render_frustum(render_state);
        render_origin(render_state);
        render_viewer(render_state);
    } else {
        xform_to_view(&world_state, render_state, &viewer_pos);
    }
}
