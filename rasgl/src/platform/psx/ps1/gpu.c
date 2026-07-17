#include "gpu.h"
#include "gpucmd.h"
#include "registers.h"
#include <assert.h>

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

void waitForVSync(void)
{
    // The GPU won't tell us directly whenever it is done sending a frame to the
    // display, but it will send a signal to another peripheral known as the
    // interrupt controller (which will be covered in a future tutorial). We can
    // thus wait until the interrupt controller's vertical blank flag gets set,
    // then reset (acknowledge) it so that it can be set again by the GPU.
    while (!(IRQ_STAT & (1 << IRQ_VSYNC)))
        __asm__ volatile("");

    IRQ_STAT = ~(1 << IRQ_VSYNC);
}

void sendLinkedList(const void* data)
{
    // Wait until the GPU's DMA unit has finished sending data and is ready.
    while (DMA_CHCR(DMA_GPU) & DMA_CHCR_ENABLE)
        __asm__ volatile("");

    // Make sure the pointer is aligned to 32 bits (4 bytes). The DMA engine is
    // not capable of reading unaligned data.
    assert(!((uint32_t)data % 4));

    // Give DMA a pointer to the beginning of the data and tell it to send it in
    // linked list mode. The DMA unit will start parsing a chain of "packets"
    // from RAM, with each packet being made up of a 32-bit header followed by
    // zero or more 32-bit commands to be sent to the GP0 register.
    DMA_MADR(DMA_GPU) = (uint32_t)data;
    DMA_CHCR(DMA_GPU) = 0
        | DMA_CHCR_WRITE
        | DMA_CHCR_MODE_LIST
        | DMA_CHCR_ENABLE;
}

void clearOrderingTable(uint32_t* table, int numEntries)
{
    DMA_MADR(DMA_OTC) = (uint32_t)&table[numEntries - 1];
    DMA_BCR(DMA_OTC) = numEntries;
    DMA_CHCR(DMA_OTC) = 0
        | DMA_CHCR_READ
        | DMA_CHCR_REVERSE
        | DMA_CHCR_MODE_BURST
        | DMA_CHCR_ENABLE
        | DMA_CHCR_TRIGGER;

    while (DMA_CHCR(DMA_OTC) & DMA_CHCR_ENABLE)
        __asm__ volatile("");
}

// As we're using an ordering table, allocatePacket() now takes the packet's Z
// index (i.e. the index of the "bucket" to link it to) as an argument. The
// table is reversed, so packets with higher Z values will be drawn first and
// between two packets with the same Z index the most recently added one will
// take precedence.
uint32_t* allocatePacket(DMAChain* chain, int zIndex, int numCommands)
{
    uint32_t* ptr = chain->nextPacket;
    chain->nextPacket += numCommands + 1;

    // Ensure the index is within valid range.
    assert((zIndex >= 0) && (zIndex < ORDERING_TABLE_SIZE));

    // Splice the new packet into the ordering table by:
    // - taking the address the ordering table entry currently points to;
    // - replacing that address with a pointer to the packet;
    // - linking the packet to the old address.
    *ptr = gp0_tag(numCommands, (void*)chain->orderingTable[zIndex]);
    chain->orderingTable[zIndex] = gp0_tag(0, ptr);

    assert(chain->nextPacket < &(chain->data)[CHAIN_BUFFER_SIZE]);

    return &ptr[1];
}
