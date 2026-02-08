#include "render.h"
#include "ps1/gpu.h"
#include "ps1/gpucmd.h"
#include "ps1/registers.h"
#include "rasgl/core/app.h"
#include "rasgl/core/debug.h"

RasResult render_renderstates_init(RenderState* states)
{
    core_renderstates_init(states);
    states[RAS_LAYER_SCENE].polygon_mode = RAS_POLYGON_WIREFRAME;

    return ras_app_renderstates_init(states);
}

void render_mesh_solid(RenderState* state)
{
    (void)state;
}

void render_mesh_wireframe(RenderState* state)
{
    RasVector4f* sv;
    for (uint32_t m = 0; m < state->num_visible_meshes; m++) {
        RasPipelineMesh* mesh = &state->meshes[state->visible_meshes[m]];

        uint32_t i = 0;
        uint32_t material_index = 0;
        while (i < mesh->num_visible_indexes) {
            int32_t material = mesh->material_indexes[material_index];
            if (material == -1) {
                ras_log_buffer("Face %d has material = %d", i / 3, material);
            }
            int8_t color = material == -1
                ? 7
                : (7 + (material * 8));
            RasPipelineVertex* pv0 = &mesh->verts[mesh->visible_indexes[i++]];
            sv = &pv0->screen_space_position;
            Point2i point0 = {
                .x = FIXED_16_16_TO_INT_32(sv->x),
                .y = FIXED_16_16_TO_INT_32(sv->y)
            };

            RasPipelineVertex* pv1 = &mesh->verts[mesh->visible_indexes[i++]];
            sv = &pv1->screen_space_position;
            Point2i point1 = {
                .x = FIXED_16_16_TO_INT_32(sv->x),
                .y = FIXED_16_16_TO_INT_32(sv->y)
            };

            RasPipelineVertex* pv2 = &mesh->verts[mesh->visible_indexes[i++]];
            sv = &pv2->screen_space_position;
            Point2i point2 = {
                .x = FIXED_16_16_TO_INT_32(sv->x),
                .y = FIXED_16_16_TO_INT_32(sv->y)
            };

            GPU_GP0 = gp0_rgb(155, 100, 0) | gp0_polyLine(false, false);
            GPU_GP0 = gp0_xy(point0.x, point0.y);
            GPU_GP0 = gp0_xy(point1.x, point1.y);
            GPU_GP0 = gp0_xy(point2.x, point2.y);
            GPU_GP0 = gp0_xy(point0.x, point0.y);
            GPU_GP0 = 0x55555555;

            material_index++;
        }
    }
}

void render_mesh_bitmap(RenderState* state)
{
    (void)state;
}

void render_clear(ScreenSettings* plat_settings)
{
    // Wait for the GPU to become ready, then send some GP0 commands to tell it
    // which area of the framebuffer we want to draw to and enable dithering.
    waitForGP0Ready();
    GPU_GP0 = gp0_texpage(0, true, false);
    GPU_GP0 = gp0_fbOffset1(0, 0);
    GPU_GP0 = gp0_fbOffset2(plat_settings->screen_width - 1, plat_settings->screen_height - 1);
    GPU_GP0 = gp0_fbOrigin(0, 0);

    // Send a VRAM fill command to quickly fill our area with solid gray.
    waitForGP0Ready();
    GPU_GP0 = gp0_rgb(64, 64, 64) | gp0_vramFill();
    GPU_GP0 = gp0_xy(0, 0);
    GPU_GP0 = gp0_xy(plat_settings->screen_width, plat_settings->screen_height);
}

void (*g_render_fns[RAS_POLYGON_COUNT])(RenderState* state) = {
    render_mesh_wireframe,
    render_mesh_solid,
    render_mesh_bitmap
};

void render_state(RenderState* state)
{
    g_render_fns[state->polygon_mode](state);
    if (state->num_commands == 0) {
        return;
    }

    ras_log_info("Rendering state with %d commands and %d points\n",
        state->num_commands,
        state->num_points);
    waitForGP0Ready();
    for (size_t i = 0; i < state->num_commands; i++) {
        RenderCommand* command = &state->commands[i];

        if (command->num_points == 1) {
            Point2i* point = &(state->points[command->point_indices[0]]);

            GPU_GP0 = gp0_rgb(155, 100, 0) | gp0_rectangle1x1(false, true, false);
            GPU_GP0 = gp0_xy(point->x, point->y);
            GPU_GP0 = gp0_rgb(255, 0, 0);
            GPU_GP0 = gp0_xy(point->x, point->y);
        }
    }
    state->current_frame++;
}
