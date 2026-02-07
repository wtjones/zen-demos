
#include "ps1/gpu.h"
#include "ps1/gpucmd.h"
#include "ps1/registers.h"
#include "rasgl/core/app.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/input.h"
#include "rasgl/core/repr.h"
#include "serial.h"
#include <stdio.h>

extern const char textData[];
extern const uint32_t textDataSize;

#define SCREEN_WIDTH 320
#define SCREEN_HEIGHT 240
ScreenSettings plat_settings
    = { .screen_width = SCREEN_WIDTH, .screen_height = SCREEN_HEIGHT };
RenderState states[RAS_LAYER_COUNT];
InputState plat_input_state;

uint32_t ras_timer_get_ticks(void)
{
    return 0;
}

void render_clear(void)
{
    // Wait for the GPU to become ready, then send some GP0 commands to tell it
    // which area of the framebuffer we want to draw to and enable dithering.
    waitForGP0Ready();
    GPU_GP0 = gp0_texpage(0, true, false);
    GPU_GP0 = gp0_fbOffset1(0, 0);
    GPU_GP0 = gp0_fbOffset2(SCREEN_WIDTH - 1, SCREEN_HEIGHT - 1);
    GPU_GP0 = gp0_fbOrigin(0, 0);

    // Send a VRAM fill command to quickly fill our area with solid gray.
    waitForGP0Ready();
    GPU_GP0 = gp0_rgb(64, 64, 64) | gp0_vramFill();
    GPU_GP0 = gp0_xy(0, 0);
    GPU_GP0 = gp0_xy(SCREEN_WIDTH, SCREEN_HEIGHT);
}

void render_state(RenderState* state)
{
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

int main(int argc, const char** argv)
{
    serial_init();
    ras_log_init();

    char buffer[256];
    int val = 99;

    if ((GPU_GP1 & GP1_STAT_FB_MODE_BITMASK) == GP1_STAT_FB_MODE_PAL) {
        puts("Using PAL mode");
        setupGPU(GP1_MODE_PAL, SCREEN_WIDTH, SCREEN_HEIGHT);
    } else {
        puts("Using NTSC mode");
        setupGPU(GP1_MODE_NTSC, SCREEN_WIDTH, SCREEN_HEIGHT);
    }

    RasResult result = ras_app_init(argc, argv, &plat_settings);
    if (result != RAS_RESULT_OK) {
        ras_log_error("Error result from ras_app_init(), exiting...");
        return 1;
    }

    core_renderstates_init(states);

    if (ras_app_renderstates_init(states) != RAS_RESULT_OK) {
        ras_log_error("Error result from ras_app_renderstates_init(), exiting...");

        return 1;
    }

    core_input_init(&plat_input_state);

    for (;;) {
        char textBuffer[256];

        for (int i = 0; i < textDataSize; i++) {
            textBuffer[i] = textData[i];
        }
        textBuffer[textDataSize] = '\0';

        val = TIMER_VALUE(1);

        int timer_mode = TIMER_CTRL_SYNC_BITMASK & TIMER_CTRL(1);
        int ret;

        ras_log_info("hello= %d, %d\n", val, timer_mode);

        ret = snprintf(buffer, sizeof(buffer), "textDataSize= %d\n%s\n", textDataSize, textBuffer);
        ras_log_info("textDataSize= %d\n%s\n", textDataSize, textBuffer);

        core_renderstates_clear(states);
        ras_core_update(&plat_input_state, states);

        for (size_t i = 0; i < RAS_LAYER_COUNT; i++) {
            states[i].screen_settings.screen_width = plat_settings.screen_width;
            states[i].screen_settings.screen_height = plat_settings.screen_height;
        }

        ras_app_render(states);
        render_clear();

        for (size_t i = 0; i < RAS_LAYER_COUNT; i++) {

            render_state(&states[i]);
            states[i].last_rasterize_ticks = 10;
        }

        // Send two GP1 commands to set the origin of the area we want to display
        // and switch on the display output.
        GPU_GP1 = gp1_fbOffset(0, 0);
        GPU_GP1 = gp1_dispBlank(false);

        for (int i = 0; i < 5000000; i++)
            __asm__ volatile("");
    }

    return 0;
}
