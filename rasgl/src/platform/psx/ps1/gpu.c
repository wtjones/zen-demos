#include "gpu.h"
#include "gpucmd.h"
#include "registers.h"
// Based on https://github.com/spicyjpeg/ps1-bare-metal/blob/main/src/01_basicGraphics/main.c

void setupGPU(GP1VideoMode mode, int width, int height)
{
    // Set the origin of the displayed framebuffer. These "magic" values,
    // derived from the GPU's internal clocks, will center the picture on most
    // displays and upscalers.
    int x = 0x760;
    int y = (mode == GP1_MODE_PAL) ? 0xa3 : 0x88;

    // Set the resolution. The GPU provides a number of fixed horizontal (256,
    // 320, 368, 512, 640) and vertical (240-256, 480-512) resolutions to pick
    // from, which affect how fast pixels are output and thus how "stretched"
    // the framebuffer will appear.
    GP1HorizontalRes horizontalRes = GP1_HRES_320;
    GP1VerticalRes verticalRes = GP1_VRES_256;

    // Set the number of displayed rows and columns. These values are in GPU
    // clock units rather than pixels, thus they are dependent on the selected
    // resolution.
    int offsetX = (width * gp1_clockMultiplierH(horizontalRes)) / 2;
    int offsetY = (height / gp1_clockDividerV(verticalRes)) / 2;

    // Hand all parameters over to the GPU by sending GP1 commands.
    GPU_GP1 = gp1_resetGPU();
    GPU_GP1 = gp1_fbRangeH(x - offsetX, x + offsetX);
    GPU_GP1 = gp1_fbRangeV(y - offsetY, y + offsetY);
    GPU_GP1 = gp1_fbMode(
        horizontalRes,
        verticalRes,
        mode,
        false,
        GP1_COLOR_16BPP);
}

void waitForGP0Ready(void)
{
    // Block until the GPU reports to be ready to accept commands through its
    // status register (which has the same address as GP1 but is read-only).
    while (!(GPU_GP1 & GP1_STAT_CMD_READY))
        __asm__ volatile("");
}
