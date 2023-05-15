#include "rasgl/core/app.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/input.h"
#include "rasgl/core/maths.h"
#include <allegro.h>

ScreenSettings plat_settings = { .screen_width = 320, .screen_height = 240 };

RenderState state = { .num_commands = 0, .num_points = 0 };
InputState plat_input_state;

void map_input()
{
    for (int i = 0; i < RAS_MAX_KEYS; i++) {
        plat_input_state.keys[i] = 0;
    }
    plat_input_state.keys[RAS_KEY_W] = key[KEY_W] ? 1 : 0;
    plat_input_state.keys[RAS_KEY_A] = key[KEY_A] ? 1 : 0;
    plat_input_state.keys[RAS_KEY_S] = key[KEY_S] ? 1 : 0;
    plat_input_state.keys[RAS_KEY_D] = key[KEY_D] ? 1 : 0;
    plat_input_state.keys[RAS_KEY_EQUALS] = key[KEY_EQUALS] ? 1 : 0;
    plat_input_state.keys[RAS_KEY_MINUS] = key[KEY_MINUS] ? 1 : 0;
}

void render_state(BITMAP* buffer, RenderState* state)
{
    for (size_t i = 0; i < state->num_commands; i++) {
        RenderCommand* command = &state->commands[i];
        for (size_t j = 0; j < command->num_points; j++) {
            Point2i* point = &(state->points[command->point_indices[j]]);
            putpixel(buffer, point->x, point->y, makecol(255, 255, 255));
        }
    }
}

int main(int argc, const char** argv)
{
    BITMAP* buffer;
    int c;

    if (allegro_init() != 0) {
        return 1;
    }

    install_keyboard();
    install_timer();

    if (set_gfx_mode(GFX_AUTODETECT, 320, 240, 0, 0) != 0) {
        set_gfx_mode(GFX_TEXT, 0, 0, 0, 0);
        allegro_message("Cannot set graphics mode:\r\n%s\r\n", allegro_error);
        return 1;
    }
    buffer = create_bitmap(SCREEN_W, SCREEN_H);

    ras_app_init(argc, argv, &plat_settings);

    set_palette(desktop_palette);
    clear_keybuf();

    while (!key[KEY_ESC]) {
        map_input();
        ras_app_update(&plat_input_state);
        ras_app_render(&state);

        clear_to_color(buffer, makecol(0, 0, 0));

        render_state(buffer, &state);

        textprintf_ex(buffer, font, 0, 0, makecol(255, 255, 255), -1,
            "Double buffered (%s) %d", gfx_driver->name);
        vsync();
        blit(buffer, screen, 0, 0, 0, 0, SCREEN_W, SCREEN_H);
    }

    destroy_bitmap(buffer);

    return 0;
}

END_OF_MAIN()
