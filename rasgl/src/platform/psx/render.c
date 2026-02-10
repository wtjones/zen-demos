#include "render.h"
#include "ps1/gpu.h"
#include "ps1/gpucmd.h"
#include "ps1/registers.h"
#include "rasgl/core/app.h"
#include "rasgl/core/color.h"
#include "rasgl/core/debug.h"

static bool using_second_frame = false;
static int frame_x = 0;
static int frame_y = 0;

RasColor colors[256] = {
    // Grey ramp (gamma corrected)
    { 0x00, 0x00, 0x00, 255 }, // #000000
    { 0x1B, 0x1B, 0x1B, 255 }, // #1B1B1B
    { 0x3A, 0x3A, 0x3A, 255 }, // #3A3A3A
    { 0x5E, 0x5E, 0x5E, 255 }, // #5E5E5E
    { 0x88, 0x88, 0x88, 255 }, // #888888
    { 0xB6, 0xB6, 0xB6, 255 }, // #B6B6B6
    { 0xE1, 0xE1, 0xE1, 255 }, // #E1E1E1
    { 0xFF, 0xFF, 0xFF, 255 }, // #FFFFFF

    // Red ramp (gamma corrected)
    { 0x00, 0x00, 0x00, 255 }, // Darkest red
    { 0x2A, 0x00, 0x00, 255 },
    { 0x4F, 0x00, 0x00, 255 },
    { 0x74, 0x00, 0x00, 255 },
    { 0x9E, 0x00, 0x00, 255 },
    { 0xC9, 0x00, 0x00, 255 },
    { 0xEC, 0x00, 0x00, 255 },
    { 0xFF, 0x00, 0x00, 255 }, // Brightest red

    // Green ramp (gamma corrected)
    { 0x00, 0x00, 0x00, 255 }, // Darkest green
    { 0x00, 0x2A, 0x00, 255 },
    { 0x00, 0x4F, 0x00, 255 },
    { 0x00, 0x74, 0x00, 255 },
    { 0x00, 0x9E, 0x00, 255 },
    { 0x00, 0xC9, 0x00, 255 },
    { 0x00, 0xEC, 0x00, 255 },
    { 0x00, 0xFF, 0x00, 255 }, // Brightest green

    // Blue ramp (gamma corrected)
    { 0x00, 0x00, 0x00, 255 }, // Darkest blue
    { 0x00, 0x00, 0x2A, 255 },
    { 0x00, 0x00, 0x4F, 255 },
    { 0x00, 0x00, 0x74, 255 },
    { 0x00, 0x00, 0x9E, 255 },
    { 0x00, 0x00, 0xC9, 255 },
    { 0x00, 0x00, 0xEC, 255 },
    { 0x00, 0x00, 0xFF, 255 } // Brightest blue
};

static RasColor* get_shaded_color(int32_t material, RasFixed diffuse_intensity)
{
    RasFixed max_shade = INT_32_TO_FIXED_16_16(7);
    RasFixed shade_fixed = mul_fixed_16_16_by_fixed_16_16(diffuse_intensity, max_shade);

    int shade = FIXED_16_16_TO_INT_32(shade_fixed);
    shade = shade < RAS_COLOR_RAMP_MIN_SHADE ? RAS_COLOR_RAMP_MIN_SHADE : shade;
    assert(shade <= 7);

    int8_t color_index = material == -1
        ? shade
        : (shade + (material * 8));

    return &colors[(uint8_t)color_index];
}

RasResult render_renderstates_init(RenderState* states)
{
    core_renderstates_init(states);
    states[RAS_LAYER_SCENE].polygon_mode = RAS_POLYGON_SOLID;
    states[RAS_LAYER_SCENE].grid_mode = RAS_GRID_MODE_OFF;

    for (int i = 0; i < RAS_LAYER_COUNT; i++) {
        states[i].max_frames = RAS_MAX_FRAMES;
    }

    return ras_app_renderstates_init(states);
}

void render_mesh_solid(RenderState* state)
{
    RasVector4f* sv;
    for (uint32_t m = 0; m < state->num_visible_meshes; m++) {
        RasPipelineMesh* mesh = &state->meshes[state->visible_meshes[m]];
        RasPipelineFace* face = &mesh->visible_faces[0];

        uint32_t i = 0;
        uint32_t material_index = 0;
        while (i < mesh->num_visible_indexes) {
            int32_t material = mesh->material_indexes[material_index];
            if (material == -1) {
                ras_log_buffer("Face %d has material = %d", i / 3, material);
            }

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

            RasColor* color_rgb = get_shaded_color(material, face->diffuse_intensity);

            GPU_GP0 = gp0_rgb(color_rgb->r, color_rgb->g, color_rgb->b) | gp0_triangle(false, false);
            GPU_GP0 = gp0_xy(point0.x, point0.y);
            GPU_GP0 = gp0_xy(point1.x, point1.y);
            GPU_GP0 = gp0_xy(point2.x, point2.y);

            material_index++;
        }
    }
}

void render_mesh_wireframe(RenderState* state)
{
    RasVector4f* sv;
    for (uint32_t m = 0; m < state->num_visible_meshes; m++) {
        RasPipelineMesh* mesh = &state->meshes[state->visible_meshes[m]];
        RasPipelineFace* face = &mesh->visible_faces[0];

        uint32_t i = 0;
        uint32_t material_index = 0;
        while (i < mesh->num_visible_indexes) {
            int32_t material = mesh->material_indexes[material_index];
            if (material == -1) {
                ras_log_buffer("Face %d has material = %d", i / 3, material);
            }

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

            RasColor* color_rgb = get_shaded_color(material, face->diffuse_intensity);
            GPU_GP0 = gp0_rgb(color_rgb->r, color_rgb->g, color_rgb->b) | gp0_polyLine(false, false);
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
    // Determine the VRAM location of the current frame. We're going to
    // place the two frames next to each other in VRAM, at (0, 0) and
    // (320, 0) respectively.
    frame_x = using_second_frame ? plat_settings->screen_width : 0;
    frame_y = 0;

    // Tell the GPU which area of VRAM belongs to the frame we're going to
    // use and enable dithering.
    waitForGP0Ready();
    GPU_GP0 = gp0_texpage(0, true, false);
    GPU_GP0 = gp0_fbOffset1(frame_x, frame_y);
    GPU_GP0 = gp0_fbOffset2(
        frame_x + plat_settings->screen_width - 1,
        frame_y + plat_settings->screen_height - 2);
    GPU_GP0 = gp0_fbOrigin(frame_x, frame_y);

    // Fill the framebuffer with solid gray.
    waitForGP0Ready();
    GPU_GP0 = gp0_rgb(64, 64, 64) | gp0_vramFill();
    GPU_GP0 = gp0_xy(frame_x, frame_y);
    GPU_GP0 = gp0_xy(plat_settings->screen_width, plat_settings->screen_height);
}

void render_flip()
{
    waitForGP0Ready();
    waitForVSync();
    GPU_GP1 = gp1_fbOffset(frame_x, frame_y);
    using_second_frame = !using_second_frame;
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
    if (state->current_frame == RAS_MAX_FRAMES) {
        ras_log_info("Reached RAS_MAX_FRAMES for layer %d.", state->layer);
    }
}
