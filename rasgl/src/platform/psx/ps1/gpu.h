#ifndef PSX_GPU_H
#define PSX_GPU_H

#include "gpucmd.h"

#define CHAIN_BUFFER_SIZE 1024 * 4
typedef struct {
    uint32_t data[CHAIN_BUFFER_SIZE];
    uint32_t* nextPacket;
} DMAChain;

void setupGPU(GP1VideoMode mode, int width, int height);
void waitForGP0Ready(void);
void waitForVSync(void);
void sendLinkedList(const void* data);
uint32_t* allocatePacket(DMAChain* chain, int numCommands);

#endif
