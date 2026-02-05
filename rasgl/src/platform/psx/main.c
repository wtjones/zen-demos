
#include "ps1/gpu.h"
#include "ps1/gpucmd.h"
#include "ps1/registers.h"
#include "rasgl/core/debug.h"
#include "serial.h"
#include <stdio.h>

extern const char textData[];
extern const uint32_t textDataSize;

#define SCREEN_WIDTH 320
#define SCREEN_HEIGHT 240

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

    // Wait for the GPU to become ready, then send some GP0 commands to tell it
    // which area of the framebuffer we want to draw to and enable dithering.
    waitForGP0Ready();
    GPU_GP0 = gp0_texpage(0, true, false);
    GPU_GP0 = gp0_fbOffset1(0, 0);
    GPU_GP0 = gp0_fbOffset2(SCREEN_WIDTH - 1, SCREEN_HEIGHT - 1);
    GPU_GP0 = gp0_fbOrigin(0, 0);

    // Send a VRAM fill command to quickly fill our area with solid gray. Note
    // that the coordinates passed to this specific command are *not* relative
    // to the ones we've just sent to the GPU!
    waitForGP0Ready();
    GPU_GP0 = gp0_rgb(64, 64, 64) | gp0_vramFill();
    GPU_GP0 = gp0_xy(0, 0);
    GPU_GP0 = gp0_xy(SCREEN_WIDTH, SCREEN_HEIGHT);

    // Tell the GPU to draw a Gouraud shaded triangle whose vertices are red,
    // green and blue respectively at the center of our drawing area.
    waitForGP0Ready();
    GPU_GP0 = gp0_rgb(255, 0, 0) | gp0_shadedTriangle(true, false, false);
    GPU_GP0 = gp0_xy(SCREEN_WIDTH / 2, 32);
    GPU_GP0 = gp0_rgb(0, 255, 0);
    GPU_GP0 = gp0_xy(32, SCREEN_HEIGHT - 32);
    GPU_GP0 = gp0_rgb(0, 0, 255);
    GPU_GP0 = gp0_xy(SCREEN_WIDTH - 32, SCREEN_HEIGHT - 32);

    // Send two GP1 commands to set the origin of the area we want to display
    // and switch on the display output.
    GPU_GP1 = gp1_fbOffset(0, 0);
    GPU_GP1 = gp1_dispBlank(false);

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

        for (int i = 0; i < 1000000; i++)
            __asm__ volatile("");
    }

    return 0;
}
