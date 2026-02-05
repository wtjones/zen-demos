#ifndef PSX_GPU_H
#define PSX_GPU_H

#include "gpucmd.h"

void setupGPU(GP1VideoMode mode, int width, int height);
void waitForGP0Ready(void);

#endif
