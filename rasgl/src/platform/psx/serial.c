/*
 * ps1-bare-metal - (C) 2023-2025 spicyjpeg
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

#include "serial.h"
#include "ps1/registers.h"

void serial_init(void)
{
    // Reset the serial interface and initialize it to output data at 115200bps,
    // 8 data bits, 1 stop bit and no parity.
    SIO_CTRL(1) = SIO_CTRL_RESET;

    SIO_MODE(1) = 0
        | SIO_MODE_BAUD_DIV1
        | SIO_MODE_DATA_8
        | SIO_MODE_STOP_1;
    SIO_BAUD(1) = F_CPU / 115200;
    SIO_CTRL(1) = 0
        | SIO_CTRL_TX_ENABLE
        | SIO_CTRL_RX_ENABLE
        | SIO_CTRL_RTS;
}

void print_char(char ch)
{
    // Wait until the serial interface is ready to send a new byte, then write
    // it to the data register.
    // NOTE: the serial interface checks for an external signal (CTS) and will
    // *not* send any data until it is asserted. To avoid blocking forever if
    // CTS is not asserted, we have to check for it manually and abort if
    // necessary.
    while (
        (SIO_STAT(1) & (SIO_STAT_TX_NOT_FULL | SIO_STAT_CTS)) == SIO_STAT_CTS)
        __asm__ volatile("");

    if (SIO_STAT(1) & SIO_STAT_CTS)
        SIO_DATA(1) = ch;
}
