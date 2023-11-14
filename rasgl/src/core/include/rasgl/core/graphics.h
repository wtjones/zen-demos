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
#define MAX_PIPELINE_VERTS 10000
#define MAX_VISIBLE_INDEXES MAX_PIPELINE_VERTS * 3

typedef enum {
    RAS_PRIMATIVE_POINTS,
    RAS_PRIMATIVE_LINES,
    RAS_PRIMATIVE_TRIANGLES
} RasPrimativeType;

typedef struct ScreenSettings {
    uint32_t screen_width;
    uint32_t screen_height;
} ScreenSettings;

typedef struct RenderCommand {
    size_t point_indices[MAX_COMMAND_POINTS];
    uint32_t num_points;
} RenderCommand;

typedef struct RasVector3fBuffer {
    int32_t* buffer;
    uint32_t num_elements;
    uint32_t max_elements;
} RasVector3fBuffer;

typedef struct RasVertex {
    RasVector3f position;
    uint8_t color;
    int32_t u, v;
    RasVector3f normal;
} RasVertex;

typedef struct RasPipelineVertex {
    RasVector3f view_space_position;
    RasVector3f view_space_normal;
    uint8_t clip_flags;
    RasVector4f screen_space_position;
    uint8_t color;
    int32_t u, v;
} RasPipelineVertex;

typedef struct RenderState {
    Point2i points[MAX_RENDER_POINTS];
    uint32_t num_points;
    RenderCommand commands[MAX_RENDER_COMMANDS];
    uint32_t num_commands;
    RasPipelineVertex pipeline_verts[MAX_PIPELINE_VERTS];
    uint32_t num_pipeline_verts;
    uint32_t visible_indexes[MAX_VISIBLE_INDEXES];
    uint32_t num_visible_indexes;
    uint32_t current_frame;
    uint32_t max_frames;
    ScreenSettings screen_settings;
} RenderState;

/**
 * Project a point in view space to screen space.
 */
Point2f project_point(int32_t screen_width, int32_t screen_height, int32_t projection_ratio, Point3f view_point);

/**
 * Transform a projected point to screen space.
 */
void projected_to_screen_point(int32_t screen_width, int32_t screen_height, int32_t projected_point[4], Point2i* screen_point);

void core_draw_elements(
    RenderState* render_state,
    RasVertex* verts,
    uint32_t num_verts,
    uint32_t* indexes,
    uint32_t num_indexes,
    int32_t model_world_matrix[4][4],
    int32_t model_view_matrix[4][4],
    int32_t proj_matrix[4][4]);

#endif
