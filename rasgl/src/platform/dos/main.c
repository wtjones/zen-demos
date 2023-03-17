#include "app.h"
#include <dpmi.h>
#include <go32.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/farptr.h>

#define MODE_13h 0x13
#define MODE_3h 0x3

void screen_mode(int mode)
{
    __dpmi_regs regs;
    regs.x.ax = mode;        /* Mode 0x13 is VGA 320x200x256, 0x3 is TEXT 80x25 */
    __dpmi_int(0x10, &regs); /* same as real-mode */
    return;
}

int main()
{
    printf("Hello!!!\n");
    screen_mode(MODE_13h);

    _farpokeb(_dos_ds, 0xA0000 + (100 * 320 + 160), 15);

    printf("init DOS...\n");
    printf("Running app_main()...\n");
    app_main();

    system("pause");
    screen_mode(MODE_3h);

    return 0;
}
