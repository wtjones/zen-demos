#ifndef PSX_GPU_H
#define PSX_GPU_H

#include "gpucmd.h"

#define DMA_MAX_CHUNK_SIZE 16
#define CHAIN_BUFFER_SIZE 1024 * 4
#define ORDERING_TABLE_SIZE 32

typedef struct {
    uint32_t data[CHAIN_BUFFER_SIZE];
    uint32_t orderingTable[ORDERING_TABLE_SIZE];
    uint32_t* nextPacket;
} DMAChain;

void setupGPU(GP1VideoMode mode, int width, int height);
void waitForGP0Ready(void);
void waitForVSync(void);
void sendLinkedList(const void* data);
void clearOrderingTable(uint32_t* table, int numEntries);
uint32_t* allocatePacket(DMAChain* chain, int numCommands);

#endif
