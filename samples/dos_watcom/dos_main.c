#include <stdio.h>
#include <string.h>
#include <i86.h>
#include <dos.h>

void initMode13(void)
{
    union REGS regs;
    struct SREGS sregs;

    regs.x.eax = 0x13;
    memset(&sregs, 0, sizeof(sregs));
    int386x(0x10, &regs, &regs, &sregs);
}

int main(int argc, char *argv[])
{
    char *screen = (char *)0xA0000;
    int x, y;
    initMode13();
    for (y = 0; y < 200; y++)
    {
        for (x = 0; x < 320; x++)
        {
            *(screen + (y * 320) + x) = y;
        }
    }
    printf("Hello, world!\n");
    sleep(2);

    return 0;
}
