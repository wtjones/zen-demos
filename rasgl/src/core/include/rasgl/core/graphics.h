#ifndef GRAPHICS_H
#define GRAPHICS_H

#include "fixed_maths.h"
#include "frustum.h"
#include "maths.h"
#include "model.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#define MAX_RENDER_POINTS 1000
#define MAX_RENDER_COMMANDS 1000
#define MAX_COMMAND_POINTS 3
#define MAX_PIPELINE_VERTS 10000
#define MAX_VISIBLE_INDEXES MAX_PIPELINE_VERTS * 3
#define RAS_MAX_AABB_POINTS 8

#define RAS_CAMERA_DEFAULT_NEAR 0.1f
#define RAS_CAMERA_DEFAULT_FAR 25.0f
#define RAS_CAMERA_DEFAULT_FOV 60.0f
#define RAS_CAMERA_DEFAULT_ASPECT_RATIO 1.333f
#define RAS_CAMERA_DEFAULT_PROJECTION_MODE RAS_PERSPECTIVE_MATRIX

typedef enum {
    RAS_PRIMATIVE_POINTS,
    RAS_PRIMATIVE_LINES,
    RAS_PRIMATIVE_TRIANGLES
} RasPrimativeType;

typedef enum {
    RAS_PERSPECTIVE_MATRIX,
    RAS_PERSPECTIVE_ALT,
    RAS_ORTHO_MATRIX
} RasProjectionMode;

typedef enum {
    RAS_BACKFACE_CULLING_ON,
    RAS_BACKFACE_CULLING_OFF,
} RasBackfaceCullingMode;

typedef enum {
    RAS_POLYGON_WIREFRAME,
    RAS_POLYGON_SOLID
} RasPolygonMode;

typedef struct ScreenSettings {
    uint32_t screen_width;
    uint32_t screen_height;
} ScreenSettings;

typedef struct RenderCommand {
    size_t point_indices[MAX_COMMAND_POINTS];
    uint32_t num_points;
} RenderCommand;

typedef struct RasVector3fBuffer {
    RasFixed* buffer;
    uint32_t num_elements;
    uint32_t max_elements;
} RasVector3fBuffer;

typedef struct RasVertex {
    RasVector3f position;
    uint8_t color;
    RasFixed u, v;
    RasVector3f normal;
} RasVertex;

typedef struct RasPipelineVertex {
    RasVector3f view_space_position;
    RasVector3f view_space_normal;
    RasClipFlags clip_flags;
    RasClipFlags aabb_clip_flags;
    RasVector4f screen_space_position;
    uint8_t color;
    RasFixed u, v;
} RasPipelineVertex;

typedef struct RasAABB {
    RasVector3f min;
    RasVector3f max;
} RasAABB;

typedef struct RasPipelineElement {
    RasVertex verts[MAX_PIPELINE_VERTS];
    uint32_t num_verts;
    uint32_t indexes[MAX_VISIBLE_INDEXES];
    uint32_t num_indexes;
    RasAABB aabb;
} RasPipelineElement;

typedef struct RasPipelineVertexBuffer {
    RasPipelineVertex verts[MAX_PIPELINE_VERTS];
    uint32_t num_verts;
    uint32_t visible_indexes[MAX_VISIBLE_INDEXES];
    uint32_t num_visible_indexes;
} RasPipelineVertexBuffer;

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
    RasProjectionMode projection_mode;
    RasBackfaceCullingMode backface_culling_mode;
    RasPolygonMode polygon_mode;
} RenderState;

void core_clip_poly(
    RasFrustum* frustum,
    RasClipFlags face_clip_flags,
    RasPipelineVertexBuffer* vert_buffer,
    uint32_t in_indexes[3]);

/**
 * @brief Sets a vector at the intersection of the given line and plane.
 *
 * @param v1 line point outside of plane
 * @param v2 line point inside of plane
 * @param plane
 * @param dest_vec
 */
void core_get_line_plane_intersect(
    RasVector3f* v1, RasVector3f* v2, RasPlane* plane, RasVector3f* dest_vec);

/**
 * Set sensible defaults
 */
void core_renderstate_init(RenderState* state);

/**
 * Clear index buffers buffers
 */
void core_renderstate_clear(RenderState* state);

void core_aabb_init(RasAABB* aabb);

/**
 * Rotate the 8 points of the AABB and generate a new AABB from the result.
 */
void core_aabb_xform(RasAABB* aabb, RasFixed matrix[4][4], RasAABB* dest);

/**
 * Sets bitmask of planes with an ouside AABB point.
 * Returns true if all points are outside.
 */
bool core_aabb_in_frustum(RasAABB* view_aabb, RasFrustum* frustum, RasClipFlags* flags);

/**
 * Transform a projected point to screen space.
 */
void projected_to_screen_point(int32_t screen_width, int32_t screen_height, RasFixed projected_point[4], Point2i* screen_point);

void core_draw_element(
    RenderState* render_state,
    RasPipelineElement* element,
    RasFixed model_world_matrix[4][4],
    RasFixed world_view_matrix[4][4],
    RasFixed proj_matrix[4][4],
    RasFrustum* frustum);

void core_model_group_to_pipeline_element(RasModelGroup* group, RasPipelineElement* element);

#endif
