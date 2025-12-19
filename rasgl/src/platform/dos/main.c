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
RenderState states[RAS_LAYER_COUNT];
InputState plat_input_state;
BITMAP* g_screen_buffer;

volatile int scancode_up = 0;
volatile int scancode_down = 0;
volatile int watcher_count = 0;

int g_key_app_to_plat[RAS_KEY_COUNT];

/**
 * @brief Track key-up events in interrupt
 *
 */
volatile RasKeyEvent g_plat_keystate[KEY_MAX];

void input_init()
{
    for (int i = 0; i < KEY_MAX; i++) {
        g_plat_keystate[i] = RAS_KEY_EVENT_NONE;
    }

    g_key_app_to_plat[RAS_KEY_A] = KEY_A;
    g_key_app_to_plat[RAS_KEY_B] = KEY_B;
    g_key_app_to_plat[RAS_KEY_C] = KEY_C;
    g_key_app_to_plat[RAS_KEY_D] = KEY_D;
    g_key_app_to_plat[RAS_KEY_E] = KEY_E;
    g_key_app_to_plat[RAS_KEY_F] = KEY_F;
    g_key_app_to_plat[RAS_KEY_G] = KEY_G;
    g_key_app_to_plat[RAS_KEY_H] = KEY_H;
    g_key_app_to_plat[RAS_KEY_I] = KEY_I;
    g_key_app_to_plat[RAS_KEY_J] = KEY_J;
    g_key_app_to_plat[RAS_KEY_K] = KEY_K;
    g_key_app_to_plat[RAS_KEY_L] = KEY_L;
    g_key_app_to_plat[RAS_KEY_M] = KEY_M;
    g_key_app_to_plat[RAS_KEY_N] = KEY_N;
    g_key_app_to_plat[RAS_KEY_O] = KEY_O;
    g_key_app_to_plat[RAS_KEY_P] = KEY_P;
    g_key_app_to_plat[RAS_KEY_Q] = KEY_Q;
    g_key_app_to_plat[RAS_KEY_R] = KEY_R;
    g_key_app_to_plat[RAS_KEY_S] = KEY_S;
    g_key_app_to_plat[RAS_KEY_T] = KEY_T;
    g_key_app_to_plat[RAS_KEY_U] = KEY_U;
    g_key_app_to_plat[RAS_KEY_V] = KEY_V;
    g_key_app_to_plat[RAS_KEY_W] = KEY_W;
    g_key_app_to_plat[RAS_KEY_X] = KEY_X;
    g_key_app_to_plat[RAS_KEY_Y] = KEY_Y;
    g_key_app_to_plat[RAS_KEY_Z] = KEY_Z;
    g_key_app_to_plat[RAS_KEY_LCTRL] = KEY_LCONTROL;
    g_key_app_to_plat[RAS_KEY_RCTRL] = KEY_RCONTROL;
    g_key_app_to_plat[RAS_KEY_LSHIFT] = KEY_LSHIFT;
    g_key_app_to_plat[RAS_KEY_RSHIFT] = KEY_RSHIFT;
    g_key_app_to_plat[RAS_KEY_MINUS] = KEY_MINUS;
    g_key_app_to_plat[RAS_KEY_EQUALS] = KEY_EQUALS;
    g_key_app_to_plat[RAS_KEY_TAB] = KEY_TAB;
    g_key_app_to_plat[RAS_KEY_ESCAPE] = KEY_ESC;
    g_key_app_to_plat[RAS_KEY_LEFTBRACKET] = KEY_OPENBRACE;
    g_key_app_to_plat[RAS_KEY_RIGHTBRACKET] = KEY_CLOSEBRACE;
    g_key_app_to_plat[RAS_KEY_UP] = KEY_UP;
    g_key_app_to_plat[RAS_KEY_DOWN] = KEY_DOWN;
    g_key_app_to_plat[RAS_KEY_LEFT] = KEY_LEFT;
    g_key_app_to_plat[RAS_KEY_RIGHT] = KEY_RIGHT;
    g_key_app_to_plat[RAS_KEY_F1] = KEY_F1;
    g_key_app_to_plat[RAS_KEY_F2] = KEY_F2;
    g_key_app_to_plat[RAS_KEY_F3] = KEY_F3;
    g_key_app_to_plat[RAS_KEY_F4] = KEY_F4;
    g_key_app_to_plat[RAS_KEY_F5] = KEY_F5;
    g_key_app_to_plat[RAS_KEY_F6] = KEY_F6;
    g_key_app_to_plat[RAS_KEY_F7] = KEY_F7;
    g_key_app_to_plat[RAS_KEY_F8] = KEY_F8;
    g_key_app_to_plat[RAS_KEY_F9] = KEY_F9;
    g_key_app_to_plat[RAS_KEY_F10] = KEY_F10;
    g_key_app_to_plat[RAS_KEY_F11] = KEY_F11;
    g_key_app_to_plat[RAS_KEY_F12] = KEY_F12;
    g_key_app_to_plat[RAS_KEY_RETURN] = KEY_ENTER;
    g_key_app_to_plat[RAS_KEY_SPACE] = KEY_SPACE;
}

void map_input()
{
    for (int i = 0; i < RAS_KEY_COUNT; i++) {

        int plat_scancode = g_key_app_to_plat[i];
        RasKeyEvent app_key_state_prior = plat_input_state.keys[i];
        RasKeyEvent plat_key_state = key[plat_scancode];
        bool plat_had_up_event = g_plat_keystate[plat_scancode] == RAS_KEY_EVENT_UP;

        // A key-up event should process for one frame only.
        // If the prior frame did not have a key-up, look at both:
        //  - the current key state
        //  - interrupt key-up events from since the last frame
        if (app_key_state_prior != RAS_KEY_EVENT_UP
            && (plat_key_state == RAS_KEY_EVENT_UP || plat_had_up_event)) {
            plat_input_state.keys[i] = RAS_KEY_EVENT_UP;
        } else {
            plat_input_state.keys[i] = plat_key_state ? RAS_KEY_EVENT_DOWN
                                                      : RAS_KEY_EVENT_NONE;
        }

        g_plat_keystate[plat_scancode] = RAS_KEY_EVENT_NONE;
    }
    plat_input_state.mods = (key_shifts & KB_CTRL_FLAG)
        ? RAS_KMOD_CTRL
        : RAS_KMOD_NONE;
    plat_input_state.mods = (key_shifts & KB_SHIFT_FLAG)
        ? plat_input_state.mods | RAS_KMOD_SHIFT
        : plat_input_state.mods;
    plat_input_state.mods = (key_shifts & KB_ALT_FLAG)
        ? plat_input_state.mods | RAS_KMOD_ALT
        : plat_input_state.mods;
}

void render_mesh_wireframe(__attribute__((unused)) RenderState* state)
{

    ras_log_buffer("Render fn2...");

    RasVector4f* sv;
    for (uint32_t m = 0; m < state->num_visible_meshes; m++) {
        RasPipelineMesh* mesh = &state->meshes[state->visible_meshes[m]];

        uint32_t i = 0;
        uint32_t material_index = 0;
        while (i < mesh->num_visible_indexes) {
            int32_t material = mesh->material_indexes[material_index];
            if (material == -1) {
                ras_log_buffer("Face %d has material = %d", i / 3, material);
            }

            // FIXME: Support palette colors
            // int8_t color = material == -1
            //     ? 7
            //     : (7 + (material * 8));

            RasPipelineVertex* pv0 = &mesh->verts[mesh->visible_indexes[i++]];
            sv = &pv0->screen_space_position;
            Point2i point0 = {
                .x = FIXED_16_16_TO_INT_32(sv->x),
                .y = FIXED_16_16_TO_INT_32(sv->y)
            };

            RasPipelineVertex* pv1 = &mesh->verts[mesh->visible_indexes[i++]];
            sv = &pv1->screen_space_position;
            Point2i point1 = {
                .x = FIXED_16_16_TO_INT_32(sv->x),
                .y = FIXED_16_16_TO_INT_32(sv->y)
            };

            RasPipelineVertex* pv2 = &mesh->verts[mesh->visible_indexes[i++]];
            sv = &pv2->screen_space_position;
            Point2i point2 = {
                .x = FIXED_16_16_TO_INT_32(sv->x),
                .y = FIXED_16_16_TO_INT_32(sv->y)
            };

            line(g_screen_buffer, point0.x, point0.y, point1.x, point1.y, makecol(0, 0, 255));
            line(g_screen_buffer, point1.x, point1.y, point2.x, point2.y, makecol(0, 255, 0));
            line(g_screen_buffer, point2.x, point2.y, point0.x, point0.y, makecol(255, 0, 0));

            material_index++;
        }
    }
}

void render_mesh_solid(__attribute__((unused)) RenderState* state)
{
}

void render_polygon_bitmap(__attribute__((unused)) RenderState* state) { }

void (*g_render_fns[RAS_POLYGON_COUNT])(RenderState* state) = {
    render_mesh_wireframe,
    render_mesh_solid,
    render_polygon_bitmap
};

void render_state(BITMAP* buffer, RenderState* state)
{
    if (!state->layer_visible) {
        return;
    }
    ras_log_buffer("Render fn...");
    g_render_fns[state->polygon_mode](state);

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

/**
 * @brief Capture key-up events via interrupt. This allows for toggle-like
 *  controls (such as modes) to trigger during low frame rate conditions.
 *
 * @param scancode
 */
void keypress_watcher(int scancode)
{
    watcher_count++;
    if (scancode & 0x80) {
        scancode_up = scancode & 0x7F;
        g_plat_keystate[scancode & 0x7F] = RAS_KEY_EVENT_UP;
    } else {
        scancode_down = scancode & 0x7F;
    }
}
END_OF_FUNCTION(keypress_watcher)

int main(int argc, const char** argv)
{
    FILE* log_file = fopen("l:\\RASGL.LOG", "w");

    log_add_fp(log_file, RAS_LOG_LEVEL_FILE);
    log_set_level(RAS_LOG_LEVEL_STRERR);
    log_set_quiet(false);

    ras_log_info("Starting Allegro...");
    if (allegro_init() != 0) {
        ras_log_error("Failed to start Allegro.");
        return 1;
    }
    if (!font) {
        ras_log_warn("Allegro font not loaded.");
        allegro_message("Font not loaded!");
        exit(1);
    }
    input_init();

    install_timer();
    LOCK_VARIABLE(scancode_up);
    LOCK_VARIABLE(scancode_down);
    LOCK_FUNCTION(keypress_watcher);
    install_keyboard();
    keyboard_lowlevel_callback = keypress_watcher;
    set_keyboard_rate(0, 0);

    if (set_gfx_mode(GFX_AUTODETECT, 320, 240, 0, 0) != 0) {
        set_gfx_mode(GFX_TEXT, 0, 0, 0, 0);
        allegro_message("Cannot set graphics mode:\r\n%s\r\n", allegro_error);
        return 1;
    }
    g_screen_buffer = create_bitmap(SCREEN_W, SCREEN_H);
    RasResult result = ras_app_init(argc, argv, &plat_settings);
    if (result != RAS_RESULT_OK) {
        ras_log_error("Error result from ras_app_init(), exiting...");
        return 1;
    }
    core_renderstates_init(states);
    states[RAS_LAYER_SCENE].polygon_mode = RAS_POLYGON_WIREFRAME;
    states[RAS_LAYER_UI].layer_visible = false;
    if (ras_app_renderstates_init(states) != RAS_RESULT_OK) {
        ras_log_error("Error result from ras_app_renderstates_init(), exiting...");
        return 1;
    }
    core_input_init(&plat_input_state);

    set_palette(desktop_palette);
    clear_keybuf();

    while (!key[KEY_ESC]) {

        map_input();

        if (keypressed()) {
            int key = readkey();
            int ascii = key & 0xFF;
            int scancode = key >> 8;

            if (ascii >= 32 && ascii <= 126) {
                ras_log_info("Text input: '%c'\n", ascii);
            } else {
                ras_log_info("Non-printable or special key: scan=%d ascii=%d", scancode, ascii);
                if (plat_input_state.keys[RAS_KEY_RETURN] == RAS_KEY_EVENT_UP) {
                    ras_log_info("Return key via state: scan=%d\n", scancode);
                }
            }
        }

        if (states[RAS_LAYER_SCENE].max_frames == UINT32_MAX
            || states[RAS_LAYER_SCENE].current_frame < states[RAS_LAYER_SCENE].max_frames) {

            core_renderstates_clear(states);
            ras_core_update(&plat_input_state, states);
            ras_app_update(&plat_input_state);

            for (size_t i = 0; i < RAS_LAYER_COUNT; i++) {
                states[i].screen_settings.screen_width = plat_settings.screen_width;
                states[i].screen_settings.screen_height = plat_settings.screen_height;
            }

            ras_app_render(states);

            clear_to_color(g_screen_buffer, makecol(0, 0, 0));

            render_state(g_screen_buffer, &states[RAS_LAYER_SCENE]);
            render_state(g_screen_buffer, &states[RAS_LAYER_UI]);
        }

        textprintf_ex(g_screen_buffer, font, 0, 0, makecol(255, 255, 255), -1,
            "Double buffered (%s)", gfx_driver->name);

        textprintf_ex(g_screen_buffer, font, 0, 10, makecol(255, 255, 255), -1,
            "Key d/u: %d - %d, %d : %s %s",
            watcher_count,
            scancode_down,
            scancode_up,
            scancode_to_name(scancode_down),
            scancode_to_name(scancode_up));

        vsync();
        blit(g_screen_buffer, screen, 0, 0, 0, 0, SCREEN_W, SCREEN_H);
    }

    destroy_bitmap(g_screen_buffer);

    return 0;
}

END_OF_MAIN()
