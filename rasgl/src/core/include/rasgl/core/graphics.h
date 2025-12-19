#ifndef GRAPHICS_H
#define GRAPHICS_H

#include "fixed_maths.h"
#include "frustum.h"
#include "maths.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#define MAX_RENDER_POINTS 10000
#define MAX_RENDER_COMMANDS 10000
#define MAX_COMMAND_POINTS 3
#define MAX_PIPELINE_VERTS 1000
#define MAX_PIPELINE_FACES (MAX_PIPELINE_VERTS)
#define MAX_VISIBLE_INDEXES (MAX_PIPELINE_VERTS * 3)

#define RAS_MESH_CLIP_PADDING_FLOOR 64 // Establish minimum padding for small meshes.
#define RAS_MESH_CLIP_PAD(x) ((x) * 2 + RAS_MESH_CLIP_PADDING_FLOOR)
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
    RAS_CLIPPING_OFF,
    RAS_CLIPPING_EXCLUDE,
    RAS_CLIPPING_MODE_COUNT
} RasClippingMode;

typedef enum {
    RAS_CLIP_SIDE_DEFAULT,
    RAS_CLIP_SIDE_ALL,
    RAS_CLIP_SIDE_NEAR_ONLY,
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

typedef enum {
    RAS_POLYGON_OUTLINE_OFF,
    /**
     * @brief Draw outlines based on outline_edges mask in face.
     *
     */
    RAS_POLYGON_OUTLINE_SPECIFIED,
    RAS_POLYGON_OUTLINE_ALL,
    RAS_POLYGON_OUTLINE_COUNT
} RasPolygonOutlineMode;

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
    RasVector4f ndc_space_position;
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
    uint8_t outline_edges;
    int32_t outline_material_index;
} RasElementFace;

typedef struct RasPipelineFace {
    RasVector3f normal;
    RasVector3f view_space_normal;
    int32_t material_index;
    RasFixed diffuse_intensity;
    RasClipFlags clip_flags;
    uint8_t outline_edges;
    int32_t outline_material_index;
} RasPipelineFace;

typedef struct RasPipelineElement {
    RasVertex* verts;
    uint32_t num_verts;
    size_t max_verts;

    RasElementFace* faces;
    uint32_t num_faces;
    size_t max_faces;

    /**
     * @brief Triangle indexes
     *  Size is number of faces * 3
     */
    uint32_t* indexes;
    uint32_t num_indexes;
    size_t max_indexes;

    int32_t* material_indexes;
    uint32_t num_material_indexes; // Will be num_indexes / 3
    size_t max_material_indexes;

    RasAABB aabb;
} RasPipelineElement;

typedef struct RasPipelineMesh {
    RasPipelineVertex* verts;
    uint32_t num_verts;
    size_t max_verts;

    uint32_t* visible_indexes;
    uint32_t num_visible_indexes;
    size_t max_visible_indexes;

    RasPipelineFace* visible_faces;
    uint32_t num_visible_faces;
    size_t max_visible_faces;

    int32_t* material_indexes;     // -1 if undefined
    uint32_t num_material_indexes; // Will be num_visible_indexes / 3
    size_t max_material_indexes;
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

    RasPipelineFace visible_faces[MAX_PIPELINE_FACES];
    uint32_t num_visible_faces;

    int32_t material_indexes[MAX_VISIBLE_INDEXES];
    uint32_t num_material_indexes; // Will be num_visible_indexes / 3

    RasPipelineMesh meshes[RAS_MAX_MESHES]; // FIXME
    uint32_t num_meshes;
    uint32_t visible_meshes[RAS_MAX_MESHES];
    uint32_t num_visible_meshes;

    uint32_t current_frame;
    uint32_t max_frames;
    uint32_t last_app_render_ticks;
    uint32_t last_rasterize_ticks;
    ScreenSettings screen_settings;
    RasRenderLayer layer;
    bool layer_visible;
    RasProjectionMode projection_mode;
    RasBackfaceCullingMode backface_culling_mode;
    RasClippingMode clipping_mode;
    RasClipSideMode clip_side_mode;
    RasPolygonMode polygon_mode;
    RasPolygonOutlineMode polygon_outline_mode;
    RasNormalMode normal_mode;
    RasGridMode grid_mode;
    RasPipelineMode pipeline_mode;
} RenderState;

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
 * @brief Set bitmask of planes with an outside AABB point.
 * Returns true if all points are outside.
 *
 * @param view_aabb
 * @param frustum
 * @param use_far_plane Prevent an all-out return if far plane is disabled.
 * @param flags Destination aabb clip flags of object.
 * @return true
 * @return false
 */
bool core_aabb_in_frustum(
    RasAABB* view_aabb,
    RasFrustum* frustum,
    bool use_far_plane,
    RasClipFlags* flags);

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

void core_get_element_aabb(RasPipelineElement* element, RasAABB* aabb);

RasResult core_pipeline_element_alloc(
    size_t max_verts,
    size_t max_faces,
    size_t max_indexes,
    size_t max_material_indexes,
    RasPipelineElement* element);

void core_pipeline_element_free(RasPipelineElement* element);

RasResult core_pipeline_mesh_alloc(
    size_t max_verts,
    size_t max_visible_indexes,
    size_t max_visible_faces,
    size_t max_material_indexes,
    RasPipelineMesh* mesh);

void core_pipeline_mesh_free(RasPipelineMesh* mesh);

void core_renderstate_free(RenderState* state);

/**
 * @brief Allocate a mesh from an allocated pipeline element.
 * Extra space is added for clipping.
 *
 * @param element
 * @param mesh
 * @return RasResult
 */
RasResult core_pipeline_element_to_mesh_alloc(
    RasPipelineElement* element,
    RasPipelineMesh* mesh);

/**
 * Add a screen-space point to the command list.
 */
void core_render_point(
    RenderState* render_state,
    RasVector4f* screen_space_position,
    int32_t material);

void core_render_line(RenderState* render_state, RasVector4f* p0, RasVector4f* p1);
#endif
