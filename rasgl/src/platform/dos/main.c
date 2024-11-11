#include "rasgl/core/app.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/input.h"
#include "rasgl/core/maths.h"
#include <allegro.h>

typedef struct {
    uint32_t ras_key;
    uint32_t plat_key;
} RasKeyMap;

ScreenSettings plat_settings
    = { .screen_width = 320, .screen_height = 240 };
RenderState state;
InputState plat_input_state;

void map_input()
{
    RasKeyMap key_maps[] = {
        { .ras_key = RAS_KEY_B, .plat_key = KEY_B },
        { .ras_key = RAS_KEY_C, .plat_key = KEY_C },
        { .ras_key = RAS_KEY_W, .plat_key = KEY_W },
        { .ras_key = RAS_KEY_A, .plat_key = KEY_A },
        { .ras_key = RAS_KEY_S, .plat_key = KEY_S },
        { .ras_key = RAS_KEY_D, .plat_key = KEY_D },
        { .ras_key = RAS_KEY_E, .plat_key = KEY_E },
        { .ras_key = RAS_KEY_Q, .plat_key = KEY_Q },
        { .ras_key = RAS_KEY_P, .plat_key = KEY_P },
        { .ras_key = RAS_KEY_F, .plat_key = KEY_F },
        { .ras_key = RAS_KEY_EQUALS, .plat_key = KEY_EQUALS },
        { .ras_key = RAS_KEY_MINUS, .plat_key = KEY_MINUS },
        { .ras_key = RAS_KEY_ESCAPE, .plat_key = KEY_ESC },
        { .ras_key = RAS_KEY_DOWN, .plat_key = KEY_DOWN },
        { .ras_key = RAS_KEY_UP, .plat_key = KEY_UP },
        { .ras_key = RAS_KEY_LEFT, .plat_key = KEY_LEFT },
        { .ras_key = RAS_KEY_RIGHT, .plat_key = KEY_RIGHT },
        { .ras_key = RAS_KEY_TAB, .plat_key = KEY_TAB },
        { .ras_key = RAS_KEY_LSHIFT, .plat_key = KEY_LSHIFT },
        { .ras_key = RAS_KEY_LCTRL, .plat_key = KEY_LCONTROL }
    };

    for (uint32_t i = 0; i < sizeof(key_maps) / sizeof(RasKeyMap); i++) {
        plat_input_state.keys[key_maps[i].ras_key] = plat_input_state.keys[key_maps[i].ras_key] == RAS_KEY_EVENT_DOWN && !key[key_maps[i].plat_key]
            ? RAS_KEY_EVENT_UP
            : key[key_maps[i].plat_key] ? RAS_KEY_EVENT_DOWN
                                        : RAS_KEY_EVENT_NONE;
    }
}

void render_state(BITMAP* buffer, RenderState* state)
{
    RasVector4f* sv;
    uint32_t i = 0;
    while (i < state->num_visible_indexes) {
        RasPipelineVertex* pv0 = &state->pipeline_verts[state->visible_indexes[i++]];
        sv = &pv0->screen_space_position;
        Point2i point0 = {
            .x = FIXED_16_16_TO_INT_32(sv->x),
            .y = FIXED_16_16_TO_INT_32(sv->y)
        };

        RasPipelineVertex* pv1 = &state->pipeline_verts[state->visible_indexes[i++]];
        sv = &pv1->screen_space_position;
        Point2i point1 = {
            .x = FIXED_16_16_TO_INT_32(sv->x),
            .y = FIXED_16_16_TO_INT_32(sv->y)
        };

        RasPipelineVertex* pv2 = &state->pipeline_verts[state->visible_indexes[i++]];
        sv = &pv2->screen_space_position;
        Point2i point2 = {
            .x = FIXED_16_16_TO_INT_32(sv->x),
            .y = FIXED_16_16_TO_INT_32(sv->y)
        };

        line(buffer, point0.x, point0.y, point1.x, point1.y, makecol(0, 0, 255));
        line(buffer, point1.x, point1.y, point2.x, point2.y, makecol(0, 255, 0));
        line(buffer, point2.x, point2.y, point0.x, point0.y, makecol(255, 0, 0));
    }

    for (size_t i = 0; i < state->num_commands; i++) {
        RenderCommand* command = &state->commands[i];
        if (command->num_points == 1) {
            Point2i* point = &(state->points[command->point_indices[0]]);
            putpixel(buffer, point->x, point->y, makecol(255, 0, 255));
        } else if (command->num_points == 2) {
            Point2i* point0 = &(state->points[command->point_indices[0]]);
            Point2i* point1 = &(state->points[command->point_indices[1]]);
            line(buffer, point0->x, point0->y, point1->x, point1->y, makecol(0, 0, 255));
        } else if (command->num_points == 3) {
            Point2i* point0 = &(state->points[command->point_indices[0]]);
            Point2i* point1 = &(state->points[command->point_indices[1]]);
            Point2i* point2 = &(state->points[command->point_indices[2]]);
            line(buffer, point0->x, point0->y, point1->x, point1->y, makecol(0, 0, 255));
            line(buffer, point1->x, point1->y, point2->x, point2->y, makecol(0, 255, 0));
            line(buffer, point2->x, point2->y, point0->x, point0->y, makecol(255, 0, 0));
        }
    }
    state->current_frame++;
}
int main(int argc, const char** argv)
{
    FILE* log_file = fopen("l:\\RASGL.LOG", "w");

    log_add_fp(log_file, RAS_LOG_LEVEL_FILE);
    log_set_level(RAS_LOG_LEVEL_STRERR);
    log_set_quiet(false);

    BITMAP* buffer;

    ras_log_info("Starting Allegro...");
    if (allegro_init() != 0) {
        ras_log_error("Failed to start Allegro.");
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

    RasResult result = ras_app_init(argc, argv, &plat_settings);
    if (result != RAS_RESULT_OK) {
        ras_log_error("Error result from ras_app_init(), exiting...");
        return 1;
    }
    core_renderstate_init(&state);
    core_input_init(&plat_input_state);

    set_palette(desktop_palette);
    clear_keybuf();

    while (!key[KEY_ESC]) {
        map_input();
        if (state.max_frames == UINT32_MAX || state.current_frame < state.max_frames) {
            core_renderstate_clear(&state);
            ras_core_update(&plat_input_state, &state);
            ras_app_update(&plat_input_state);
            state.screen_settings.screen_width = plat_settings.screen_width;
            state.screen_settings.screen_height = plat_settings.screen_height;
            ras_app_render(&state);

            clear_to_color(buffer, makecol(0, 0, 0));
            render_state(buffer, &state);
        }

        textprintf_ex(buffer, font, 0, 0, makecol(255, 255, 255), -1,
            "Double buffered (%s)", gfx_driver->name);
        vsync();
        blit(buffer, screen, 0, 0, 0, 0, SCREEN_W, SCREEN_H);
    }

    destroy_bitmap(buffer);

    return 0;
}

END_OF_MAIN()
