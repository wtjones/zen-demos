
#include "controller.h"
#include "input.h"
#include "ps1/gpu.h"
#include "ps1/gpucmd.h"
#include "ps1/registers.h"
#include "rasgl/core/app.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/input.h"
#include "rasgl/core/repr.h"
#include "rasgl/core/scene.h"
#include "render.h"
#include "serial.h"
#include <malloc.h>
#include <stdio.h>

extern const char textData[];
extern const uint32_t textDataSize;
extern int frame_x;
extern int frame_y;
extern DMAChain dma_chains[2];
extern DMAChain* chain;
extern bool using_second_frame;

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

int main(int argc, const char** argv)
{
    serial_init();
    initControllerBus();

    ras_log_init();

    char buffer[256];
    int val = 99;

    RasScene* scene;
    if (core_load_scene("bld_psx/scene.mp", &scene) != RAS_RESULT_OK) {
        ras_log_error("Error loading scene, exiting...");
        goto fail;
    }

    ras_log_info("PSX heap usage: %d bytes. Free: %d bytes\n",
        mallinfo2().uordblks, mallinfo2().fordblks);

    if ((GPU_GP1 & GP1_STAT_FB_MODE_BITMASK) == GP1_STAT_FB_MODE_PAL) {
        puts("Using PAL mode");
        setupGPU(GP1_MODE_PAL, SCREEN_WIDTH, SCREEN_HEIGHT);
    } else {
        puts("Using NTSC mode");
        setupGPU(GP1_MODE_NTSC, SCREEN_WIDTH, SCREEN_HEIGHT);
    }

    // Turn on the video output.
    DMA_DPCR |= DMA_DPCR_CH_ENABLE(DMA_GPU);
    GPU_GP1 = gp1_dmaRequestMode(GP1_DREQ_GP0_WRITE);
    GPU_GP1 = gp1_dispBlank(false);

    RasResult result = ras_app_init(argc, argv, &plat_settings);
    if (result != RAS_RESULT_OK) {
        ras_log_error("Error result from ras_app_init(), exiting...");
        goto fail;
    }

    ras_log_info("PSX heap usage: %d bytes\n", mallinfo2().uordblks);

    if (render_renderstates_init(states) != RAS_RESULT_OK) {
        ras_log_error("Error result from render_renderstates_init(), exiting...");
        goto fail;
    }

    ras_log_info("PSX heap usage: %d bytes\n", mallinfo2().uordblks);
    core_input_init(&plat_input_state);
    input_init();

    ras_log_info("RAS_MAX_FRAMES: %d\n", RAS_MAX_FRAMES);

    using_second_frame = false;

    for (;;) {
        char textBuffer[256];

        for (int i = 0; i < textDataSize; i++) {
            textBuffer[i] = textData[i];
        }
        textBuffer[textDataSize] = '\0';

        val = TIMER_VALUE(1);
        int timer_mode = TIMER_CTRL_SYNC_BITMASK & TIMER_CTRL(1);

        frame_x = using_second_frame ? plat_settings.screen_width : 0;
        frame_y = 0;
        chain = &dma_chains[using_second_frame ? 1 : 0];
        using_second_frame = !using_second_frame;

        // Display the frame that was just drawn by the GPU (if any). We are
        // going to overwrite its respective DMA chain afterwards, as the GPU no
        // longer needs it.
        GPU_GP1 = gp1_fbOffset(frame_x, frame_y);

        // Reset the chain for this frame.
        chain->nextPacket = chain->data;

        render_clear(&plat_settings);

        if (states[RAS_LAYER_SCENE].max_frames == UINT32_MAX
            || states[RAS_LAYER_SCENE].current_frame < states[RAS_LAYER_SCENE].max_frames) {

            core_renderstates_clear(states);
            ras_core_update(&plat_input_state, states);
            ras_app_update(&plat_input_state);

            for (size_t i = 0; i < RAS_LAYER_COUNT; i++) {
                states[i].screen_settings.screen_width = plat_settings.screen_width;
                states[i].screen_settings.screen_height = plat_settings.screen_height;
            }

            ras_app_render(states);

            for (size_t i = 0; i < RAS_LAYER_COUNT; i++) {

                render_state(&states[i]);
                states[i].last_rasterize_ticks = 10;
            }
        }

        // Terminate the DMA chain
        *(chain->nextPacket) = gp0_endTag(0);

        input_map();
        render_flip();
    }
    ras_log_flush();
    return 0;

fail:
    ras_log_info("PSX heap usage: %d bytes. Free: %d bytes\n",
        mallinfo2().uordblks, mallinfo2().fordblks);
    return 1;
}
