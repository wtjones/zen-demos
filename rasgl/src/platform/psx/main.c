
#include "ps1/registers.h"
#include "rasgl/core/debug.h"
#include "serial.h"
#include <stdio.h>

extern const char textData[];
extern const uint32_t textDataSize;

int main(int argc, const char** argv)
{
    serial_init();
    ras_log_init();

    char buffer[256];
    int val = 99;

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
