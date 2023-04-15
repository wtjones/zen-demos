#include "app.h"
#include <allegro.h>

int main(int argc, const char** argv)
{
    if (allegro_init() != 0) {
        return 1;
    }

    install_keyboard();

    if (set_gfx_mode(GFX_AUTODETECT, 320, 200, 0, 0) != 0) {
        set_gfx_mode(GFX_TEXT, 0, 0, 0, 0);
        allegro_message("Cannot set graphics mode:\r\n%s\r\n", allegro_error);
        return 1;
    }

    set_palette(desktop_palette);
    clear_to_color(screen, makecol(255, 255, 255));
    int text_w = 0, text_h = 0;
    textout_ex(screen, font, "init DOS...", text_w, text_h, makecol(0, 0, 0), -1);
    text_h += text_height(font);

    textout_ex(screen, font, "Running ras_app_init()...", text_w, text_h, makecol(0, 0, 0), -1);
    ras_app_init(argc, argv);

    readkey();

    return 0;
}

END_OF_MAIN()
