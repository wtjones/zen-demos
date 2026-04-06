/**
 * Minimal PSX exe for testing memory usage of ras core
 *
 */
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
#include "usage.h"
// clang-format off
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <malloc.h>
// clang-format on
#include <stdio.h>
#include <stdlib.h>

extern const char textData[];
extern const uint32_t textDataSize;
extern int frame_x;
extern int frame_y;
extern DMAChain dma_chains[2];
extern DMAChain* chain;
extern bool using_second_frame;

/* Linker-provided section symbols (from cmake-psx/executable.ld) */
extern char _textStart[];
extern char _textEnd[];
extern char _dataStart[];
extern char _dataEnd[];
extern char _bssStart[];
extern char _bssEnd[];

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

    psx_log_usage_info();

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

    psx_log_heap();
    psx_log_usage_info();

    using_second_frame = false;

    // allocate memory in chunks 16k until cannot
    {
        size_t chunk_size = 16 * 1024;
        size_t total_allocated = 0;
        while (true) {
            void* ptr = malloc(chunk_size);
            if (ptr == NULL) {
                break;
            }
            total_allocated += chunk_size;
        }
        ras_log_info("Total allocated memory in 16k chunks until OOM: %zu bytes", total_allocated);
        psx_log_usage_info();
        psx_log_heap();
    }

    for (;;) {
        char textBuffer[256];

        for (int i = 0; i < textDataSize; i++) {
            textBuffer[i] = textData[i];
        }
        textBuffer[textDataSize] = '\0';

        val = TIMER_VALUE(1);
        int timer_mode = TIMER_CTRL_SYNC_BITMASK & TIMER_CTRL(1);

        frame_x = using_second_frame ? 320 : 0;
        frame_y = 0;
        chain = &dma_chains[using_second_frame ? 1 : 0];
        using_second_frame = !using_second_frame;

        // Display the frame that was just drawn by the GPU (if any). We are
        // going to overwrite its respective DMA chain afterwards, as the GPU no
        // longer needs it.
        GPU_GP1 = gp1_fbOffset(frame_x, frame_y);

        // Reset the chain for this frame.
        chain->nextPacket = chain->data;

        // Terminate the DMA chain
        *(chain->nextPacket) = gp0_endTag(0);

        render_flip();
    }
    ras_log_flush();
    return 0;

fail:
    psx_log_heap();
    return 1;
}
