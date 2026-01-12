
#include "ps1/registers.h"
#include "serial.h"
#include <stdio.h>

extern const char textData[];
extern const uint32_t textDataSize;

int main(int argc, const char** argv)
{

    serial_init();

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
        int ret = snprintf(buffer, sizeof(buffer), "hello= %d, %d\n", val, timer_mode);

        for (int i = 0; i < ret; i++)
            print_char(buffer[i]);

        ret = snprintf(buffer, sizeof(buffer), "textDataSize= %d\n%s\n", textDataSize, textBuffer);

        for (int i = 0; i < ret; i++)
            print_char(buffer[i]);

        for (int i = 0; i < 1000000; i++)
            __asm__ volatile("");
    }

    return 0;
}
