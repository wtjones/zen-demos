/*
 * ps1-bare-metal - (C) 2023 spicyjpeg
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

#pragma once

/* Minimal PSX controller API used by the platform layer. */

#include <stdbool.h>
#include <stdint.h>

typedef enum RasPlatButton {
    PLAT_BUTTON_SELECT = 0,
    PLAT_BUTTON_L3 = 1,
    PLAT_BUTTON_R3 = 2,
    PLAT_BUTTON_START = 3,
    PLAT_BUTTON_UP = 4,
    PLAT_BUTTON_RIGHT = 5,
    PLAT_BUTTON_DOWN = 6,
    PLAT_BUTTON_LEFT = 7,
    PLAT_BUTTON_L2 = 8,
    PLAT_BUTTON_R2 = 9,
    PLAT_BUTTON_L1 = 10,
    PLAT_BUTTON_R1 = 11,
    PLAT_BUTTON_TRIANGLE = 12,
    PLAT_BUTTON_CIRCLE = 13,
    PLAT_BUTTON_X = 14,
    PLAT_BUTTON_SQUARE = 15,
    PLAT_BUTTON_COUNT = 16
} RasPlatButton;

void delayMicroseconds(int time);

/* Initialize the PSX controller serial bus. */
void initControllerBus(void);

uint16_t getControllerInfo(int port);

/*
 * Fill `output` with a human-readable description of the controller
 * connected to `port` (0-based). The buffer should be large enough to
 * hold the result (caller uses 256 bytes).
 */
void printControllerInfo(int port, char* output);
