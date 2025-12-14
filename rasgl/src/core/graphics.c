#include "rasgl/core/graphics.h"
#include "rasgl/core/clip.h"
#include "rasgl/core/color.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/event.h"
#include "rasgl/core/normals.h"
#include "rasgl/core/repr.h"

void core_aabb_init(RasAABB* aabb)
{
    aabb->min.x = RAS_FIXED_MAX;
    aabb->min.y = RAS_FIXED_MAX;
    aabb->min.z = RAS_FIXED_MAX;

    aabb->max.x = RAS_FIXED_MIN;
    aabb->max.y = RAS_FIXED_MIN;
    aabb->max.z = RAS_FIXED_MIN;
}

void core_aabb_to_points(RasAABB* aabb, RasVector3f points[8])
{
    // Considering right-handed system (see README.md), min is point 4, max is point 3.
    //
    //     6- - - - -  7
    //    /|          /|
    // 2/ -|- - - -3/  |
    // |   |       |   |
    // |   |       |   |
    // |   4- - - -| - 5
    // |  /        |  /
    // 0/ - - - - -1/

    points[0].x = aabb->min.x;
    points[0].y = aabb->min.y;
    points[0].z = aabb->max.z;

    points[1].x = aabb->max.x;
    points[1].y = aabb->min.y;
    points[1].z = aabb->max.z;

    points[2].x = aabb->min.x;
    points[2].y = aabb->max.y;
    points[2].z = aabb->max.z;

    points[3].x = aabb->max.x;
    points[3].y = aabb->max.y;
    points[3].z = aabb->max.z;

    points[4].x = aabb->min.x;
    points[4].y = aabb->min.y;
    points[4].z = aabb->min.z;

    points[5].x = aabb->max.x;
    points[5].y = aabb->min.y;
    points[5].z = aabb->min.z;

    points[6].x = aabb->min.x;
    points[6].y = aabb->max.y;
    points[6].z = aabb->min.z;

    points[7].x = aabb->max.x;
    points[7].y = aabb->max.y;
    points[7].z = aabb->min.z;
}

void core_aabb_xform(RasAABB* aabb, RasFixed matrix[4][4], RasAABB* dest)
{
    RasFixed vec_src[4];
    RasFixed vec_dest[4];

    RasVector3f points[RAS_MAX_AABB_POINTS];
    RasVector3f points_rotated[RAS_MAX_AABB_POINTS];
    core_aabb_init(dest);

    // Rotate the 8 points of the box to get the full extent of the resulting box
    core_aabb_to_points(aabb, points);

    for (int i = 0; i < RAS_MAX_AABB_POINTS; i++) {
        core_vector3f_to_4x1(&points[i], vec_src);
        mat_mul_4x4_4x1(matrix, vec_src, vec_dest);
        core_4x1_to_vector3f(vec_dest, &points_rotated[i]);

        dest->min.x = vec_dest[0] < dest->min.x
            ? vec_dest[0]
            : dest->min.x;
        dest->min.y = vec_dest[1] < dest->min.y
            ? vec_dest[1]
            : dest->min.y;
        dest->min.z = vec_dest[2] < dest->min.z
            ? vec_dest[2]
            : dest->min.z;

        dest->max.x = vec_dest[0] > dest->max.x
            ? vec_dest[0]
            : dest->max.x;
        dest->max.y = vec_dest[1] > dest->max.y
            ? vec_dest[1]
            : dest->max.y;
        dest->max.z = vec_dest[2] > dest->max.z
            ? vec_dest[2]
            : dest->max.z;
    }
}

bool core_aabb_in_frustum(
    RasAABB* aabb,
    RasFrustum* frustum,
    bool use_far_plane,
    RasClipFlags* flags)
{
    RasVector3f points[RAS_MAX_AABB_POINTS];
    RasClipFlags point_flags[RAS_MAX_AABB_POINTS];
    *flags = 0;
    core_aabb_to_points(aabb, points);

    for (int i = 0; i < RAS_MAX_AABB_POINTS; i++) {
        point_flags[i] = core_point_in_frustum_planes(frustum, &points[i]);
        *flags = *flags | point_flags[i];
    }

    // Check if all AABB points are outside a plane.
    int32_t num_frustum_planes = (use_far_plane == true ? FRUSTUM_PLANES : FRUSTUM_PLANES - 1);
    for (RasFrustumPlane fpi = 0; fpi < num_frustum_planes; fpi++) {

        RasClipFlags plane_flag = core_to_clip_flag(fpi);
        bool all_out = true;

        for (int i = 0; i < RAS_MAX_AABB_POINTS; i++) {
            if (!(point_flags[i] & plane_flag)) {
                all_out = false;
                break;
            }
        }

        if (all_out) {
            return true;
        }
    }

    return false;
}

void core_renderstate_init(RenderState* state)
{
    state->num_commands = 0;
    state->num_points = 0;
    state->num_pipeline_verts = 0;
    state->num_visible_indexes = 0;
    state->num_visible_faces = 0;
    state->num_material_indexes = 0;
    state->num_meshes = 0;
    state->num_visible_meshes = 0;
    state->current_frame = 0;
    state->max_frames = UINT32_MAX;
    state->last_app_render_ticks = 0;
    state->last_rasterize_ticks = 0;

    state->backface_culling_mode = RAS_BACKFACE_CULLING_ON;
    state->clipping_mode = RAS_CLIPPING_ON;
    state->clip_side_mode = RAS_CLIP_SIDE_DEFAULT;
    state->polygon_outline_mode = RAS_POLYGON_OUTLINE_SPECIFIED;
    state->normal_mode = RAS_NORMAL_MODE_OFF;
    state->grid_mode = RAS_GRID_MODE_OFF;
    state->pipeline_mode = RAS_PIPELINE_MODE_DEFAULT;

    memset(state->material_indexes, 0, sizeof(state->material_indexes));
    memset(state->visible_indexes, 0, sizeof(state->visible_indexes));
    memset(state->visible_faces, 0, sizeof(state->visible_faces));
    memset(state->visible_meshes, 0, sizeof(state->visible_meshes));
};

void core_renderstate_scene_init(RenderState* state)
{
    core_renderstate_init(state);
    state->layer = RAS_LAYER_SCENE;
    state->layer_visible = true;
    state->projection_mode = RAS_PERSPECTIVE_MATRIX;
    state->polygon_mode = RAS_POLYGON_SOLID;
    state->grid_mode = RAS_GRID_MODE_ORIGIN;
}

void core_renderstate_ui_init(RenderState* state)
{
    core_renderstate_init(state);
    state->layer = RAS_LAYER_UI;
    state->layer_visible = true;
    state->projection_mode = RAS_ORTHO_MATRIX;
    state->polygon_mode = RAS_POLYGON_BITMAP;
}

void core_renderstate_console_init(RenderState* state)
{
    core_renderstate_init(state);
    state->layer = RAS_LAYER_CONSOLE;
    state->layer_visible = false;
    state->projection_mode = RAS_ORTHO_MATRIX;
    state->polygon_mode = RAS_POLYGON_BITMAP;
}

void core_renderstates_init(RenderState states[])
{
    core_renderstate_scene_init(&states[RAS_LAYER_SCENE]);
    core_renderstate_ui_init(&states[RAS_LAYER_UI]);
    core_renderstate_console_init(&states[RAS_LAYER_CONSOLE]);
}

void core_renderstate_clear(RenderState* state)
{
    state->num_commands = 0;
    state->num_points = 0;
    state->num_pipeline_verts = 0;
    state->num_visible_indexes = 0;
    state->num_material_indexes = 0;
    state->num_visible_faces = 0;
    memset(state->material_indexes, 0, sizeof(state->material_indexes));
    memset(state->visible_indexes, 0, sizeof(state->visible_indexes));
    memset(state->visible_faces, 0, sizeof(state->visible_faces));
    state->num_meshes = 0;
    memset(state->meshes, 0, sizeof(state->meshes));
}

void core_renderstates_clear(RenderState states[])
{
    for (size_t i = 0; i < RAS_LAYER_COUNT; i++) {
        core_renderstate_clear(&states[i]);
    }
}

void projected_to_screen_point(RasFixed screen_width, RasFixed screen_height, RasFixed projected_point[4], Point2i* screen_point)
{

    RasFixed half_screen_width = INT_32_TO_FIXED_16_16(screen_width / 2);
    RasFixed half_screen_height = INT_32_TO_FIXED_16_16(screen_height / 2);

    screen_point->x = FIXED_16_16_TO_INT_32(
        mul_fixed_16_16_by_fixed_16_16(half_screen_width, projected_point[0]) + half_screen_width);
    screen_point->y = FIXED_16_16_TO_INT_32(
        mul_fixed_16_16_by_fixed_16_16(half_screen_height, projected_point[1]) + half_screen_height);
}

bool core_is_backface(const RasVector4f* sv0, const RasVector4f* sv1, const RasVector4f* sv2)
{
    // norm1 = (1.x - 0.x) * (0.y - 2.y)
    // norm2 = (1.y - 0.y) * (0.x - 2.x)
    int64_t dx1 = (int64_t)(sv1->x - sv0->x);
    int64_t dy1 = (int64_t)(sv1->y - sv0->y);
    int64_t dx2 = (int64_t)(sv2->x - sv0->x);
    int64_t dy2 = (int64_t)(sv2->y - sv0->y);

    int64_t cross = mul_fixed_16_16_to_fixed_32_32(dx1, dy2)
        - mul_fixed_16_16_to_fixed_32_32(dy1, dx2);

    return cross < 0;
}

bool core_get_line_plane_intersect(
    RasVector3f* v1, RasVector3f* v2, RasPlane* plane, RasVector3f* dest_vec)
{
    RasFixed pv_a_side = core_dot_product(v1, &plane->normal) + plane->distance;
    RasFixed pv_b_side = core_dot_product(v2, &plane->normal) + plane->distance;

    // Handle edge cases gracefully
    if (pv_a_side == 0) {
        *dest_vec = *v1;
        return true;
    }
    if (pv_b_side == 0) {
        *dest_vec = *v2;
        return true;
    }

    // For intersection to be valid, vertices must be on opposite sides
    if ((pv_a_side > 0 && pv_b_side > 0) || (pv_a_side < 0 && pv_b_side < 0)) {
        ras_log_buffer_warn_ex(
            RAS_EVENT_INVALID_MATH,
            "Warning: both vertices on same side of plane");

        return false;
    }

    RasFixed scale = div_fixed_16_16_by_fixed_16_16(-pv_a_side, pv_b_side - pv_a_side);

    RasVector3f* cur_vert = v1;
    RasVector3f* next_vert = v2;
    dest_vec->x = cur_vert->x + (mul_fixed_16_16_by_fixed_16_16(next_vert->x - cur_vert->x, scale));
    dest_vec->y = cur_vert->y + (mul_fixed_16_16_by_fixed_16_16(next_vert->y - cur_vert->y, scale));
    dest_vec->z = cur_vert->z + (mul_fixed_16_16_by_fixed_16_16(next_vert->z - cur_vert->z, scale));
    return true;
}

void core_render_point(
    RenderState* render_state,
    RasVector4f* screen_space_position,
    int32_t material)
{
    uint32_t* num_points = &render_state->num_points;
    uint32_t* num_commands = &render_state->num_commands;
    Point2i* screen_pos;

    if (*num_points >= MAX_RENDER_POINTS || *num_commands >= MAX_RENDER_COMMANDS) {
        ras_log_error("Render state full, cannot render more points.");
        assert(false);
        return;
    }
    screen_pos = &render_state->points[*num_points];
    screen_pos->x = FIXED_16_16_TO_INT_32(screen_space_position->x);
    screen_pos->y = FIXED_16_16_TO_INT_32(screen_space_position->y);

    render_state->commands[*num_commands].num_points = 1;
    render_state->commands[*num_commands].point_indices[0] = *num_points;
    RasFixed shade_scale = float_to_fixed_16_16(3.5);

    RasFixed darken = mul_fixed_16_16_by_fixed_16_16(
        screen_space_position->z, shade_scale);
    uint8_t darken_8 = FIXED_16_16_TO_INT_32(darken) < (RAS_COLOR_RAMP_SIZE - 1)
        ? FIXED_16_16_TO_INT_32(darken)
        : 6;

    uint8_t shade = material + RAS_COLOR_RAMP_SIZE - 1 - darken_8;
    render_state->commands[*num_commands].color = shade;
    (*num_points)++;
    (*num_commands)++;
}

void core_render_line(RenderState* render_state, RasVector4f* p0, RasVector4f* p1)
{
    uint32_t* num_points = &render_state->num_points;
    uint32_t* num_commands = &render_state->num_commands;
    Point2i* screen_pos;

    screen_pos = &render_state->points[*num_points];
    screen_pos->x = FIXED_16_16_TO_INT_32(p0->x);
    screen_pos->y = FIXED_16_16_TO_INT_32(p0->y);

    render_state->commands[*num_commands].num_points = 2;
    render_state->commands[*num_commands].point_indices[0] = *num_points;
    (*num_points)++;
    screen_pos++;

    screen_pos->x = FIXED_16_16_TO_INT_32(p1->x);
    screen_pos->y = FIXED_16_16_TO_INT_32(p1->y);
    render_state->commands[*num_commands].point_indices[1] = *num_points;
    (*num_points)++;
    (*num_commands)++;
}

void core_projected_to_screen_point(int32_t screen_width, int32_t screen_height, RasFixed projected_point[4], RasVector4f* screen_point)
{
    RasFixed half_screen_width = INT_32_TO_FIXED_16_16(screen_width / 2);
    RasFixed half_screen_height = INT_32_TO_FIXED_16_16(screen_height / 2);

    screen_point->x = mul_fixed_16_16_by_fixed_16_16(half_screen_width, projected_point[0]) + half_screen_width;
    screen_point->y = -mul_fixed_16_16_by_fixed_16_16(half_screen_height, projected_point[1]) + half_screen_height;
    screen_point->z = projected_point[2];
    screen_point->w = projected_point[3];
}

void core_render_aabb(
    RenderState* render_state,
    RasFixed proj_matrix[4][4],
    RasFrustum* frustum,
    RasAABB* aabb)
{
    RasVector3f points[RAS_MAX_AABB_POINTS];
    core_aabb_to_points(aabb, points);
    uint32_t num_points = 0;
    for (int i = 0; i < RAS_MAX_AABB_POINTS; i++) {
        if (!core_point_in_frustum(frustum, &points[i])) {
            continue;
        }
        RasFixed view_vec[4];
        RasFixed projected_vec[4];
        RasVector4f screen_space_position;

        core_vector3f_to_4x1(&points[i], view_vec);

        // Screen space in NDC coords
        mat_mul_project(proj_matrix, view_vec, projected_vec);

        core_projected_to_screen_point(
            render_state->screen_settings.screen_width,
            render_state->screen_settings.screen_height,
            projected_vec,
            &screen_space_position);

        core_render_point(render_state, &screen_space_position, RAS_COLOR_RAMP_OFFSET_RED);
        num_points++;
    }
    ras_log_buffer("AABB points rendered: %d\n", num_points);
}

void core_light_poly(
    RasPipelineFace* face,
    RasVector3f* camera_pos,
    RasVector3f* light_pos)
{
    RasVector3f light_dir;
    core_sub_vector3f(light_pos, camera_pos, &light_dir);
    core_normalize(&light_dir);

    face->diffuse_intensity = core_dot_product(
        &face->view_space_normal, &light_dir);

    face->diffuse_intensity = face->diffuse_intensity < 0 ? 0 : face->diffuse_intensity;
    char buffer[255];
    ras_log_buffer_trace("diffuse_intensity: %s", repr_fixed_16_16(buffer, sizeof buffer, face->diffuse_intensity));
}

void core_get_element_aabb(RasPipelineElement* element, RasAABB* aabb)
{
    core_aabb_init(aabb);

    for (int i = 0; i < element->num_verts; i++) {
        RasVertex* element_vert = &element->verts[i];
        core_min_vector3f(&element->aabb.min, &element_vert->position, &element->aabb.min);
        core_max_vector3f(&element->aabb.max, &element_vert->position, &element->aabb.max);
    }

    char buffer[255];
    ras_log_trace("Model AABB min: %s\n", repr_point3f(buffer, sizeof buffer, &element->aabb.min));
    ras_log_trace("Model AABB max: %s\n", repr_point3f(buffer, sizeof buffer, &element->aabb.max));
}

void core_model_group_to_pipeline_element(RasModelGroup* group, RasPipelineElement* element)
{

    element->num_verts = group->num_verts;
    for (int i = 0; i < group->num_verts; i++) {
        RasVertex* element_vert = &element->verts[i];

        element_vert->position.x = group->verts[i].x;
        element_vert->position.y = group->verts[i].y;
        element_vert->position.z = group->verts[i].z;
    }

    core_get_element_aabb(element, &element->aabb);

    element->num_indexes = group->num_faces * 3;
    element->num_material_indexes = group->num_faces;
    element->num_faces = group->num_faces;
    uint32_t* element_index = &element->indexes[0];
    int32_t* material_index = &element->material_indexes[0];
    RasElementFace* dest_face = &element->faces[0];

    for (int j = 0; j < group->num_faces; j++) {
        RasModelFace* face = &group->faces[j];
        for (int k = 0; k < RAS_MAX_MODEL_FACE_INDEXES; k++) {
            RasModelFaceIndex* face_index = &face->indexes[k];
            *element_index = face_index->vert_index;
            element_index++;
        }

        *material_index = face->material_index;
        material_index++;

        dest_face->material_index = face->material_index;
        RasVector3f* src_normal = &group->normals[face->indexes[0].normal_index];
        dest_face->normal = *src_normal;
        dest_face++;
    }
}
