#include "ps1/gpu.h"
#include "ps1/gte.h"
#include "rasgl/core/repr.h"
#include "rasgl/core/stages.h"
#include "render.h"
#include "repr.h"
#include "stages.h"
#include <stdlib.h>

#define RAS_PSX_VERT_SCALE 64
#define RAS_PSX_ONE (1 << 12)

extern DMAChain dma_chains[2];
extern DMAChain* chain;

static inline RasFixed gte_scale_to_fixed(int16_t gte)
{
    return (RasFixed)((gte << 16) / RAS_PSX_VERT_SCALE);
}

/**
 * @brief Pack to a 16 bit scaled vertex for the GTE.
 * Y/Z flipped due to match the GTE coordinate system.
 * See also research/psx.md
 * @param v
 * @return GTEVector16
 */
static inline GTEVector16 vert3f_to_gte_vertex(const RasVector3f* v)
{
    return (GTEVector16) {
        .x = (int16_t)((v->x * RAS_PSX_VERT_SCALE) >> 16),
        .y = -(int16_t)((v->y * RAS_PSX_VERT_SCALE) >> 16),
        .z = -(int16_t)((v->z * RAS_PSX_VERT_SCALE) >> 16),
    };
}

static inline GTEVector16 fixed_to_gte_vector(const RasVector3f* v)
{
    return (GTEVector16) {
        .x = (int16_t)((v->x * RAS_PSX_VERT_SCALE) >> 16),
        .y = (int16_t)((v->y * RAS_PSX_VERT_SCALE) >> 16),
        .z = (int16_t)((v->z * RAS_PSX_VERT_SCALE) >> 16),
    };
}

static inline void psx_angle_to_sin_cos(int16_t* s, int16_t* c, int32_t angle)
{
    char buffer[255];
    *s = (int16_t)-RAS_FIXED_16_16_TO_20_12(
        RAS_SIN(angle));
    *c = (int16_t)RAS_FIXED_16_16_TO_20_12(
        RAS_COS(angle));
    ras_log_buffer_info("PSX sin: %s", repr_fixed_16_16(buffer, sizeof(buffer), RAS_SIN(angle)));
}

void psx_mat_rotate(GTEMatrix* multiplied, RasVector3f* rotation)
{
    int32_t angle;
    int16_t s, c;
    char buffer[255];

    // Rotate X
    angle = FIXED_16_16_TO_INT_32(rotation->x);
    psx_angle_to_sin_cos(&s, &c, angle);

    gte_setColumnVectors(
        c, -s, 0,
        s, c, 0,
        0, 0, RAS_PSX_ONE);

    multiplyCurrentMatrixByVectors(multiplied);

    ras_log_buffer_info("PSX matrix x: %s",
        repr_gte_matrix(buffer, sizeof(buffer), multiplied));
    gte_loadRotationMatrix(multiplied);

    // Rotate Y
    angle = FIXED_16_16_TO_INT_32(rotation->y);
    psx_angle_to_sin_cos(&s, &c, angle);

    gte_setColumnVectors(
        c, 0, s,
        0, RAS_PSX_ONE, 0,
        -s, 0, c);

    multiplyCurrentMatrixByVectors(multiplied);
    ras_log_buffer_info("PSX matrix y: %s",
        repr_gte_matrix(buffer, sizeof(buffer), multiplied));
    gte_loadRotationMatrix(multiplied);

    // Rotate Z
    angle = FIXED_16_16_TO_INT_32(rotation->z);
    ras_log_buffer_info("psx angle z: %d", angle);
    psx_angle_to_sin_cos(&s, &c, angle);

    gte_setColumnVectors(
        RAS_PSX_ONE, 0, 0,
        0, c, -s,
        0, s, c);

    multiplyCurrentMatrixByVectors(multiplied);
    ras_log_buffer_info("PSX matrix z: %s",
        repr_gte_matrix(buffer, sizeof(buffer), multiplied));
    gte_loadRotationMatrix(multiplied);
}

void psx_camera_world_view_init(RasPSXMatrix* multiplied, RasCamera* camera)
{
    RasFixed translate_to_viewer[4][4];

    int32_t angle = (camera->angle + 180) % 360;
    if (angle < 0) {
        angle += 360;
    }

    RasVector3f rotation = { .x = 0, .y = INT_32_TO_FIXED_16_16(angle), .z = 0 };
    char buffer[255];
    ras_log_buffer_info("cam to psx rot: %s",
        repr_point3f(buffer, sizeof(buffer), &rotation));
    psx_mat_rotate(&multiplied->m, &rotation);

    // Combine world to viewer translate and rotate operations
    Point3f trans_pos = {
        -camera->position.x,
        -camera->position.y,
        -camera->position.z
    };

    GTEVector16 trv = fixed_to_gte_vector(&trans_pos);

    ras_log_buffer_info(
        "camera trans_pos: %s",
        repr_point3f(buffer, sizeof buffer, &trans_pos));

    ras_log_buffer_info(
        "camera trv: [%d, %d, %d]",
        trv.x, trv.y, trv.z);

    // Load V0 with camera pos
    gte_setDataReg(
        GTE_VXY0,
        ((uint32_t)(uint16_t)trv.x)
            | ((uint32_t)(uint16_t)trv.y << 16));

    gte_setDataReg(GTE_VZ0, trv.z);

    // Calculate the translation component of the WVM.
    // Basically the translation part of the 4x4 in core pipeline.
    gte_command(
        GTE_CMD_MVMVA | GTE_SF | GTE_MX_RT | GTE_V_V0 | GTE_CV_NONE);

    multiplied->translation.x = (int16_t)gte_getDataReg(GTE_IR1);
    multiplied->translation.y = (int16_t)gte_getDataReg(GTE_IR2);
    multiplied->translation.z = (int16_t)gte_getDataReg(GTE_IR3);

    RasVector3f result = {
        .x = gte_scale_to_fixed(multiplied->translation.x),
        .y = gte_scale_to_fixed(multiplied->translation.y),
        .z = gte_scale_to_fixed(multiplied->translation.z),
    };

    ras_log_buffer_info("psx world view result: %s\n",
        repr_point3f(buffer, sizeof buffer, &result));
}

/**
 * @brief Create scene world view matrix.
 *
 * @param input
 * @return void*
 */
void* psx_sg_setup(void* input)
{
    char buffer[1000];
    RasRenderData* render_data = (RasRenderData*)input;
    if (render_data == NULL || render_data->scene == NULL || render_data->camera == NULL) {
        ras_log_error("Invalid render data or scene/camera not set.");
        return NULL;
    }

    if (!render_data->plat) {
        render_data->plat = malloc(sizeof(RasPSXRenderData));
        if (!render_data->plat) {
            ras_log_error("Unable to malloc() PSX render data");
            return NULL;
        }
    }
    RasPSXRenderData* psx = render_data->plat;

    // Set identity to registers
    gte_setRotationMatrix(
        RAS_PSX_ONE, 0, 0,
        0, RAS_PSX_ONE, 0,
        0, 0, RAS_PSX_ONE);

    // PSX: Set world view matrix
    psx_camera_world_view_init(&psx->world_view_matrix, render_data->camera);

    ras_log_buffer_info("Core world_view_matrix: %s",
        repr_mat_4x4(buffer, sizeof buffer, render_data->world_view_matrix));

    ras_log_buffer_info("PSX world_view_matrix: %s",
        repr_gte_matrix(buffer, sizeof(buffer), &psx->world_view_matrix.m));

    return render_data;
}

/**
 * @brief Create per-object:
 *  - model world matrix
 *  - model view matrix
 *
 * @param input
 * @return void*
 */
void* psx_sg_xform_objects(void* input)
{
    char buffer[1000];
    RasRenderData* render_data = (RasRenderData*)input;
    RasPSXRenderData* psx = render_data->plat;

    for (size_t i = 0; i < render_data->scene->num_objects; i++) {
        RasSceneObject* current_object = &render_data->scene->objects[i];

        uint32_t mesh_index = current_object->mesh_index;
        RasVector3f* model_pos = &current_object->position;
        RasVector3f* model_rotation = &current_object->rotation;
        RasFixed(*model_world_matrix)[4] = render_data->model_world_matrix[mesh_index];
        RasFixed(*model_view_matrix)[4] = render_data->model_view_matrix[mesh_index];
        RasFixed(*normal_mvt_matrix)[4] = render_data->normal_mvt_matrix[mesh_index];

        //
        // PSX: Build model world matrix - rotation
        //

        RasPSXMatrix* psx_wvm = &psx->world_view_matrix;
        RasPSXMatrix* psx_mwm = &psx->model_world_matrix[mesh_index];
        RasPSXMatrix* psx_mvm = &psx->model_view_matrix[mesh_index];

        // Set identity to registers
        gte_setRotationMatrix(
            RAS_PSX_ONE, 0, 0,
            0, RAS_PSX_ONE, 0,
            0, 0, RAS_PSX_ONE);

        psx_mat_rotate(&psx_mwm->m, model_rotation);

        //
        // PSX: Build model world matrix - translation
        //

        ras_log_buffer_info("PSX: model pos: %s\n",
            repr_point3f(buffer, sizeof buffer, model_pos));
        // Combine model to world translate and rotate operations
        Point3f trans_pos = {
            -model_pos->x,
            -model_pos->y,
            -model_pos->z
        };

        GTEVector16 trv = vert3f_to_gte_vertex(&trans_pos);

        psx_mwm->translation.x = trv.x;
        psx_mwm->translation.y = trv.y;
        psx_mwm->translation.z = trv.z;

        RasVector3f result = {
            .x = gte_scale_to_fixed(psx_mwm->translation.x),
            .y = gte_scale_to_fixed(psx_mwm->translation.y),
            .z = gte_scale_to_fixed(psx_mwm->translation.z),
        };

        ras_log_buffer_info("PSX: model world translation result: %s\n",
            repr_point3f(buffer, sizeof buffer, &result));

        ras_log_buffer_info("Core Model world matrix: %s",
            repr_mat_4x4(buffer, sizeof buffer, model_world_matrix));
        ras_log_buffer_info("PSX Model world matrix: %s",
            repr_gte_matrix(buffer, sizeof(buffer), &psx_mwm->m));

        //
        // PSX: Model view matrix - rotation
        // MVM = world view matrix * model world matrix
        //
        // Here: How to multiple the two matricies?

        // Load world view matrix from camera
        ras_log_buffer_info("BEFORE -- PSX world_view_matrix: %s",
            repr_gte_matrix(buffer, sizeof(buffer), &psx_wvm->m));

        gte_loadRotationMatrix(&psx_wvm->m);

        // Load model world matrix to registers

        ras_log_buffer_info("BEFORE -- PSX Model view matrix: %s",
            repr_gte_matrix(buffer, sizeof(buffer), &psx_mvm->m));
        gte_setColumnVectors(
            psx_mwm->m.values[0][0], psx_mwm->m.values[0][1], psx_mwm->m.values[0][2],
            psx_mwm->m.values[1][0], psx_mwm->m.values[1][1], psx_mwm->m.values[1][2],
            psx_mwm->m.values[2][0], psx_mwm->m.values[2][1], psx_mwm->m.values[2][2]);

        multiplyCurrentMatrixByVectors(&psx_mvm->m);

        ras_log_buffer_info("Core Model view matrix: %s",
            repr_mat_4x4(buffer, sizeof buffer, model_view_matrix));
        ras_log_buffer_info("PSX Model view matrix: %s",
            repr_gte_matrix(buffer, sizeof(buffer), &psx_mvm->m));

        //
        // PSX: Model view matrix - translation
        // MVM translation = R_w × T_m + T_w
        //
        // Load V0 with model world translation
        gte_setDataReg(
            GTE_VXY0,
            ((uint32_t)(uint16_t)psx_mwm->translation.x)
                | ((uint32_t)(uint16_t)psx_mwm->translation.y << 16));

        gte_setDataReg(GTE_VZ0, psx_mwm->translation.z);

        // Calculate R_w * T_m
        gte_command(
            GTE_CMD_MVMVA | GTE_SF | GTE_MX_RT | GTE_V_V0 | GTE_CV_NONE);

        psx_mvm->translation.x = (int16_t)gte_getDataReg(GTE_IR1);
        psx_mvm->translation.y = (int16_t)gte_getDataReg(GTE_IR2);
        psx_mvm->translation.z = (int16_t)gte_getDataReg(GTE_IR3);

        result.x = gte_scale_to_fixed(psx_mvm->translation.x);
        result.y = gte_scale_to_fixed(psx_mvm->translation.y);
        result.z = gte_scale_to_fixed(psx_mvm->translation.z);

        ras_log_buffer_info("psx model view translation result before add: %s\n",
            repr_point3f(buffer, sizeof buffer, &result));

        // Calculate (R_w × T_m ) + T_w
        psx_mvm->translation.x += psx_wvm->translation.x;
        psx_mvm->translation.y += psx_wvm->translation.y;
        psx_mvm->translation.z += psx_wvm->translation.z;

        result.x = gte_scale_to_fixed(psx_mvm->translation.x);
        result.y = gte_scale_to_fixed(psx_mvm->translation.y);
        result.z = gte_scale_to_fixed(psx_mvm->translation.z);

        ras_log_buffer_info("psx model view translation result: %s\n",
            repr_point3f(buffer, sizeof buffer, &result));
    }
}

void psx_aabb_xform(RasAABB* aabb, GTEMatrix* matrix, RasAABB* dest)
{
    RasFixed vec_src[4];
    RasFixed vec_dest[4];

    RasVector3f points[RAS_MAX_AABB_POINTS];
    RasVector3f points_rotated[RAS_MAX_AABB_POINTS];
    core_aabb_init(dest);

    // Rotate the 8 points of the box to get the full extent of the resulting box
    core_aabb_to_points(aabb, points);

    gte_loadRotationMatrix(matrix);

    for (int i = 0; i < RAS_MAX_AABB_POINTS; i++) {

        GTEVector16 gte_point = vert3f_to_gte_vertex(&points[i]);

        gte_setDataReg(
            GTE_VXY0,
            ((uint32_t)(uint16_t)gte_point.x) | ((uint32_t)(uint16_t)gte_point.y << 16));
        gte_setDataReg(GTE_VZ0, gte_point.z);

        gte_command(
            GTE_CMD_MVMVA | GTE_SF | GTE_MX_RT | GTE_V_V0 | GTE_CV_TR);

        uint32_t x = (int16_t)gte_getDataReg(GTE_IR1);
        uint32_t y = (int16_t)gte_getDataReg(GTE_IR2);
        uint32_t z = (int16_t)gte_getDataReg(GTE_IR3);

        RasVector3f result = {
            .x = gte_scale_to_fixed(x),
            .y = gte_scale_to_fixed(y),
            .z = gte_scale_to_fixed(z),
        };

        char buffer[255];
        if (i == 0) {

            ras_log_buffer_info("psx AABB result: %s\n",
                repr_point3f(buffer, sizeof buffer, &result));
        }

        continue;
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

void* psx_sg_xform_aabb(void* input)
{
    char buffer[1000];
    RasRenderData* render_data = (RasRenderData*)input;
    RasPSXRenderData* psx = render_data->plat;

    for (size_t i = 0; i < render_data->scene->num_objects; i++) {
        RasSceneObject* current_object = &render_data->scene->objects[i];
        RasPipelineElement* element = &render_data->scene->models[current_object->model_index].element;
        const uint32_t mesh_index = current_object->mesh_index;
        RasAABB* view_aabb = &render_data->aabbs[mesh_index];

        GTEMatrix* psx_mvm = &psx->model_view_matrix[mesh_index].m;

        // FIXME: Use translation component
        psx_aabb_xform(
            &element->aabb,
            psx_mvm,
            view_aabb);
    }
}

void* psx_sg_xform_verts(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;
    size_t xformed_verts = 0;

    for (uint32_t i = 0; i < render_data->num_mesh_elements; i++) {
        uint32_t mesh_index = render_data->mesh_elements[i].mesh_index;
        RasPipelineElement* element = render_data->mesh_elements[i].element_ref;
        RasPipelineMesh* mesh = &render_data->render_state->meshes[mesh_index];

        RasFixed(*model_world_matrix)[4] = render_data->model_world_matrix[mesh_index];
        RasFixed(*model_view_matrix)[4] = render_data->model_view_matrix[mesh_index];
        RasFixed(*normal_mvt_matrix)[4] = render_data->normal_mvt_matrix[mesh_index];

        mesh->num_verts = element->num_verts;

        gte_setControlReg(GTE_TRX, 0);
        gte_setControlReg(GTE_TRY, 0);
        gte_setControlReg(GTE_TRZ, 128);
        gte_setRotationMatrix(
            RAS_PSX_ONE, 0, 0,
            0, RAS_PSX_ONE, 0,
            0, 0, RAS_PSX_ONE);

        static GTEMatrix multiplied;

        // Load rotation into registers
        RasSceneObject* current_object = &render_data->scene->objects[0];

        int32_t angle = FIXED_16_16_TO_INT_32(current_object->rotation.y);

        RasFixed s = -RAS_FIXED_16_16_TO_20_12(
            RAS_SIN(angle));
        RasFixed c = RAS_FIXED_16_16_TO_20_12(
            RAS_COS(angle));

        char buffer[255];
        ras_log_buffer_info("PSX sin: %s", repr_fixed_16_16(buffer, sizeof(buffer), RAS_SIN(angle)));
        int16_t s1 = (int16_t)s;
        int16_t c1 = (int16_t)c;

        gte_setColumnVectors(
            c1, 0, s1,
            0, RAS_PSX_ONE, 0,
            -s1, 0, c1);

        ras_log_buffer_info("PSX matrix1: %s", repr_gte_matrix(buffer, sizeof(buffer), &multiplied));
        multiplyCurrentMatrixByVectors(&multiplied);
        ras_log_buffer_info("PSX matrix2: %s", repr_gte_matrix(buffer, sizeof(buffer), &multiplied));
        gte_loadRotationMatrix(&multiplied);
        ras_log_buffer_info("PSX matrix3: %s", repr_gte_matrix(buffer, sizeof(buffer), &multiplied));

        int16_t backfacing = 0;
        uint32_t material_index = 0;
        uint32_t face_index = 0;
        for (uint32_t i = 0; i < element->num_indexes; i += 3) {
            int32_t material = mesh->material_indexes[material_index];
            RasPipelineVertex* pv1 = &mesh->verts[element->indexes[i]];
            RasPipelineVertex* pv2 = &mesh->verts[element->indexes[i + 1]];
            RasPipelineVertex* pv3 = &mesh->verts[element->indexes[i + 2]];

            RasVector3f* v0 = &element->verts[element->indexes[i]].position;
            RasVector3f* v1 = &element->verts[element->indexes[i + 1]].position;
            RasVector3f* v2 = &element->verts[element->indexes[i + 2]].position;

            char buffer[255];
            ras_log_buffer_info("psx v0: %s", repr_point3f(buffer, sizeof(buffer), v0));
            ras_log_buffer_info("psx v1: %s", repr_point3f(buffer, sizeof(buffer), v1));
            ras_log_buffer_info("psx v2: %s", repr_point3f(buffer, sizeof(buffer), v2));

            int32_t temp = v2->x;
            ras_log_buffer_info("psx start: %d", temp);
            temp = v2->x * RAS_PSX_VERT_SCALE;
            ras_log_buffer_info("psx scale: %d", temp);

            GTEVector16 gv0 = vert3f_to_gte_vertex(v0);
            gte_loadV0(&gv0);
            GTEVector16 gv1 = vert3f_to_gte_vertex(v1);
            gte_loadV1(&gv1);
            GTEVector16 gv2 = vert3f_to_gte_vertex(v2);
            gte_loadV2(&gv2);
            ras_log_buffer_info("psx gv0: %d %d %d", gv0.x, gv0.y, gv0.z);
            ras_log_buffer_info("psx gv1: %d %d %d", gv1.x, gv1.y, gv1.z);
            ras_log_buffer_info("psx gv2: %d %d %d", gv2.x, gv2.y, gv2.z);

            gte_command(GTE_CMD_RTPT | GTE_SF);
            gte_command(GTE_CMD_NCLIP);

            if ((int32_t)gte_getDataReg(GTE_MAC0) >= 0) {
                backfacing++;
                continue;
            }

            uint32_t xy0 = gte_getDataReg(GTE_SXY0);
            uint32_t xy1 = gte_getDataReg(GTE_SXY1);
            uint32_t xy2 = gte_getDataReg(GTE_SXY2);
            ras_log_buffer_info("psx result: X: %d, Y: %d", xy0 >> 16, xy0 & 0x0000FFFF);
            ras_log_buffer_info("psx result: X: %d, Y: %d", xy1 >> 16, xy1 & 0x0000FFFF);
            ras_log_buffer_info("psx result: X: %d, Y: %d", xy2 >> 16, xy2 & 0x0000FFFF);

            int16_t x0 = (int16_t)(xy0 & 0xffff);
            int16_t y0 = (int16_t)(xy0 >> 16);

            int16_t x1 = (int16_t)(xy1 & 0xffff);
            int16_t y1 = (int16_t)(xy1 >> 16);

            int16_t x2 = (int16_t)(xy2 & 0xffff);
            int16_t y2 = (int16_t)(xy2 >> 16);

            ras_log_buffer_info("psx result: X: %d, Y: %d", x0, y0);
            ras_log_buffer_info("psx result: X: %d, Y: %d", x1, y1);
            ras_log_buffer_info("psx result: X: %d, Y: %d", x2, y2);

            // fixme: use diffuse
            RasColor* color_rgb = get_shaded_color(material, 7);
            uint32_t* ptr;

            if (render_data->render_state->polygon_mode == RAS_POLYGON_SOLID) {
                ptr = allocatePacket(chain, ORDERING_TABLE_SIZE - 4, 4);
                ptr[0] = gp0_rgb(color_rgb->r, color_rgb->g, color_rgb->b) | gp0_triangle(false, false);
                ptr[1] = xy0;
                ptr[2] = xy1;
                ptr[3] = xy2;
            } else {
                ptr = allocatePacket(chain, ORDERING_TABLE_SIZE - 4, 6);
                ptr[0] = gp0_rgb(32, 0, 0) | gp0_polyLine(false, false);
                ptr[1] = xy0;
                ptr[2] = xy1;
                ptr[3] = xy2;
                ptr[4] = xy0;
                ptr[5] = 0x55555555;
            }

            material_index++;
            face_index++;
        }

        ras_log_buffer_info("PSX: Backfacing: %d", backfacing);
    }
    ras_log_buffer(
        "Transformed %d verts",
        xformed_verts);
}

void psx_pipeline_init(RasPipeline* pipeline)
{
    pipeline->num_stages = 0;

    ADD_STAGE(pipeline, core_sg_setup);
    ADD_STAGE(pipeline, psx_sg_setup);

    ADD_STAGE(pipeline, core_sg_xform_tombmaps);

    ADD_STAGE(pipeline, core_sg_xform_objects);
    ADD_STAGE(pipeline, psx_sg_xform_objects);

    ADD_STAGE(pipeline, core_sg_xform_tombmap_aabb);
    ADD_STAGE(pipeline, psx_sg_xform_aabb);
    ADD_STAGE(pipeline, core_sg_xform_aabb);
    ADD_STAGE(pipeline, core_sg_render_aabb);

    ADD_STAGE(pipeline, core_sg_xform_verts);
    ADD_STAGE(pipeline, psx_sg_xform_verts);

    ADD_STAGE(pipeline, core_sg_project_to_clip_space);
    ADD_STAGE(pipeline, core_sg_clip_flag_verts);
    ADD_STAGE(pipeline, core_sg_visible_faces);
    ADD_STAGE(pipeline, core_sg_project_to_screen_space);
    ADD_STAGE(pipeline, core_sg_cull_backfaces);
    ADD_STAGE(pipeline, core_sg_xform_normals);
    ADD_STAGE(pipeline, core_sg_lighting);
    ADD_STAGE(pipeline, core_sg_draw_normals);
    ADD_STAGE(pipeline, core_sg_draw_grid);

    ras_log_buffer_info("Pipeline initialized with %d stages", pipeline->num_stages);
}
