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
#define RAS_MAX_MESHES 10 // FIXME

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
    RAS_CLIPPING_ON,
    RAS_CLIPPING_ALT,
    RAS_CLIPPING_OFF,
    RAS_CLIPPING_EXCLUDE,
    RAS_CLIPPING_MODE_COUNT
} RasClippingMode;

typedef enum {
    RAS_CLIP_SIDE_SS,
    RAS_CLIP_SIDE_VS,
    RAS_CLIP_SIDE_MODE_COUNT
} RasClipSideMode;

typedef enum {
    RAS_NORMAL_MODE_OFF,
    RAS_NORMAL_MODE_FAUX,
    RAS_NORMAL_MODE_ORTHO,
    RAS_NORMAL_MODE_COUNT
} RasNormalMode;

typedef enum {
    RAS_POLYGON_WIREFRAME,
    RAS_POLYGON_SOLID,
    RAS_POLYGON_BITMAP,
    RAS_POLYGON_COUNT
} RasPolygonMode;

/**
 * @brief Grid output flags
 *
 */
typedef enum {
    RAS_GRID_MODE_OFF = 0x00,
    RAS_GRID_MODE_ORIGIN = 0x01,
    RAS_GRID_MODE_GRID = 0x02,
    RAS_GRID_MODE_COUNT = 3
} RasGridMode;

typedef enum {
    RAS_PIPELINE_MODE_OFF,     // Use class pipeline (draw elements)
    RAS_PIPELINE_MODE_DEFAULT, // Use staged pipeline
    RAS_PIPELINE_MODE_COUNT
} RasPipelineMode;

typedef enum {
    RAS_LAYER_SCENE,
    RAS_LAYER_UI,
    RAS_LAYER_CONSOLE,
    RAS_LAYER_COUNT
} RasRenderLayer;

typedef struct ScreenSettings {
    uint32_t screen_width;
    uint32_t screen_height;
} ScreenSettings;

typedef struct RenderCommand {
    size_t point_indices[MAX_COMMAND_POINTS];
    uint32_t num_points;
    uint8_t color;
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
    RasVector4f clip_space_position;
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

typedef struct RasElementFace {
    RasVector3f normal;
    int32_t material_index;
} RasElementFace;

typedef struct RasPipelineFace {
    RasVector3f normal;
    RasVector3f view_space_normal;
    int32_t material_index;
    RasFixed diffuse_intensity;
    RasClipFlags clip_flags;
} RasPipelineFace;

typedef struct RasPipelineElement {
    RasVertex verts[MAX_PIPELINE_VERTS];
    uint32_t num_verts;

    RasElementFace faces[MAX_PIPELINE_VERTS / 3];
    uint32_t num_faces;

    /**
     * @brief Triangle indexes
     *  Size is number of faces * 3
     */
    uint32_t indexes[MAX_VISIBLE_INDEXES];
    uint32_t num_indexes;

    int32_t material_indexes[MAX_VISIBLE_INDEXES];
    uint32_t num_material_indexes; // Will be num_indexes / 3

    RasAABB aabb;
} RasPipelineElement;

typedef struct RasPipelineVertexBuffer {
    RasPipelineVertex verts[MAX_PIPELINE_VERTS];
    uint32_t num_verts;

    uint32_t visible_indexes[MAX_VISIBLE_INDEXES];
    uint32_t num_visible_indexes;

    RasPipelineFace visible_faces[MAX_PIPELINE_VERTS / 3];
    uint32_t num_visible_faces;

    int32_t material_indexes[MAX_VISIBLE_INDEXES]; // -1 if undefined
    uint32_t num_material_indexes;                 // Will be num_visible_indexes / 3
} RasPipelineVertexBuffer;

typedef struct RasPipelineMesh {
    RasPipelineVertex verts[MAX_PIPELINE_VERTS];
    uint32_t num_verts;

    uint32_t visible_indexes[MAX_VISIBLE_INDEXES];
    uint32_t num_visible_indexes;

    RasPipelineFace visible_faces[MAX_PIPELINE_VERTS / 3];
    uint32_t num_visible_faces;

    int32_t material_indexes[MAX_VISIBLE_INDEXES]; // -1 if undefined
    uint32_t num_material_indexes;                 // Will be num_visible_indexes / 3
} RasPipelineMesh;

typedef struct RenderState {
    Point2i points[MAX_RENDER_POINTS];
    uint32_t num_points;

    RenderCommand commands[MAX_RENDER_COMMANDS];
    uint32_t num_commands;

    RasPipelineVertex pipeline_verts[MAX_PIPELINE_VERTS];
    uint32_t num_pipeline_verts;

    uint32_t visible_indexes[MAX_VISIBLE_INDEXES];
    uint32_t num_visible_indexes;

    RasPipelineFace visible_faces[MAX_PIPELINE_VERTS / 3];
    uint32_t num_visible_faces;

    int32_t material_indexes[MAX_VISIBLE_INDEXES];
    uint32_t num_material_indexes; // Will be num_visible_indexes / 3

    RasPipelineMesh meshes[RAS_MAX_MESHES]; // FIXME
    uint32_t num_meshes;
    uint32_t visible_meshes[RAS_MAX_MESHES];
    uint32_t num_visible_meshes;

    uint32_t current_frame;
    uint32_t max_frames;
    ScreenSettings screen_settings;
    RasRenderLayer layer;
    bool layer_visible;
    RasProjectionMode projection_mode;
    RasBackfaceCullingMode backface_culling_mode;
    RasClippingMode clipping_mode;
    RasClipSideMode clip_side_mode;
    RasPolygonMode polygon_mode;
    RasNormalMode normal_mode;
    RasGridMode grid_mode;
    RasPipelineMode pipeline_mode;
} RenderState;

void core_clip_poly(
    RasFrustum* frustum,
    RasClipFlags face_clip_flags,
    RasPipelineVertexBuffer* vert_buffer,
    uint32_t in_indexes[3],
    int32_t material_index,
    RasPipelineFace* face);

/**
 * @brief Sets a vector at the intersection of the given line and plane.
 *
 * @param v1 line point outside of plane
 * @param v2 line point inside of plane
 * @param plane
 * @param dest_vec
 * @returns true if intersection is valid
 */
bool core_get_line_plane_intersect(
    RasVector3f* v1, RasVector3f* v2, RasPlane* plane, RasVector3f* dest_vec);

/**
 * Set sensible defaults
 */
void core_renderstate_init(RenderState* state);

/**
 * Set sensible defaults for all layer states
 */
void core_renderstates_init(RenderState states[]);

/**
 * Clear index buffers buffers
 */
void core_renderstate_clear(RenderState* state);

/**
 * Clear index buffers buffers
 */
void core_renderstates_clear(RenderState states[]);

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
 * Determine if poly is backfacing based on the normal's angle to the viewer
 * in screen space. Assumes vertices are counter-clockwise.
 * Based on https://github.com/wtjones/qbasic/blob/master/POLY3D.BAS
 */
bool core_is_backface_ss(RasVector4f* sv0, RasVector4f* sv1, RasVector4f* sv2);

/**
 * Determine if poly is backfacing based on the normal's angle to the viewer
 * in clip space. Assumes vertices are counter-clockwise.
 * Based on https://github.com/wtjones/qbasic/blob/master/POLY3D.BAS
 */
bool core_is_backface(const RasVector4f* v0, const RasVector4f* v1, const RasVector4f* v2);

void core_projected_to_screen_point(
    int32_t screen_width,
    int32_t screen_height,
    RasFixed projected_point[4],
    RasVector4f* screen_point);

void core_light_poly(
    RasPipelineFace* face,
    RasVector3f* camera_pos,
    RasVector3f* light_pos);

/**
 * Transform a projected point to screen space.
 */
void projected_to_screen_point(
    int32_t screen_width,
    int32_t screen_height,
    RasFixed projected_point[4],
    Point2i* screen_point);

void core_draw_element(
    RenderState* render_state,
    RasPipelineElement* element,
    RasFixed model_world_matrix[4][4],
    RasFixed world_view_matrix[4][4],
    RasFixed proj_matrix[4][4],
    RasFrustum* frustum);

void core_model_group_to_pipeline_element(RasModelGroup* group, RasPipelineElement* element);

/**
 * Add a screen-space point to the command list.
 */
void core_render_point(
    RenderState* render_state,
    RasVector4f* screen_space_position,
    int32_t material);

void core_render_line(RenderState* render_state, RasVector4f* p0, RasVector4f* p1);
#endif
