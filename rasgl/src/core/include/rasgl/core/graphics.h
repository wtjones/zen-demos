#ifndef GRAPHICS_H
#define GRAPHICS_H

#include "maths.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#define MAX_RENDER_POINTS 1000
#define MAX_RENDER_COMMANDS 1000
#define MAX_COMMAND_POINTS 3

typedef struct ScreenSettings {
    int32_t screen_width;
    int32_t screen_height;
} ScreenSettings;

typedef struct RenderCommand {
    size_t point_indices[MAX_COMMAND_POINTS];
    int32_t num_points;
} RenderCommand;

typedef struct RenderState {
    Point2i points[MAX_RENDER_POINTS];
    size_t num_points;
    RenderCommand commands[MAX_RENDER_COMMANDS];
    size_t num_commands;
} RenderState;

#endif
