#ifndef GRAPHICS_H
#define GRAPHICS_H

#include "fixed_maths.h"
#include "maths.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#define MAX_RENDER_POINTS 1000
#define MAX_RENDER_COMMANDS 1000
#define MAX_COMMAND_POINTS 3

typedef struct ScreenSettings {
    uint32_t screen_width;
    uint32_t screen_height;
} ScreenSettings;

typedef struct RenderCommand {
    size_t point_indices[MAX_COMMAND_POINTS];
    uint32_t num_points;
} RenderCommand;

typedef struct RenderState {
    Point2i points[MAX_RENDER_POINTS];
    uint32_t num_points;
    RenderCommand commands[MAX_RENDER_COMMANDS];
    uint32_t num_commands;
    uint32_t current_frame;
} RenderState;

/**
 * Project a point in view space to screen space.
 */
Point2f project_point(int32_t screen_width, int32_t screen_height, int32_t projection_ratio, Point3f view_point);

#endif
