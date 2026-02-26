#include "input.h"
#include "controller.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/input.h"

int g_plat_to_app[PLAT_BUTTON_COUNT] = { 0 };
extern InputState plat_input_state;

void input_init()
{
    // Movement - WASD
    g_plat_to_app[PLAT_BUTTON_UP] = RAS_KEY_W;
    g_plat_to_app[PLAT_BUTTON_DOWN] = RAS_KEY_S;
    g_plat_to_app[PLAT_BUTTON_LEFT] = RAS_KEY_A;
    g_plat_to_app[PLAT_BUTTON_RIGHT] = RAS_KEY_D;

    // Rotation
    g_plat_to_app[PLAT_BUTTON_SQUARE] = RAS_KEY_Q;
    g_plat_to_app[PLAT_BUTTON_CIRCLE] = RAS_KEY_E;

    // Up/down
    g_plat_to_app[PLAT_BUTTON_R1] = RAS_KEY_Z;
    g_plat_to_app[PLAT_BUTTON_R2] = RAS_KEY_C;
}

void input_map()
{
    int16_t buttons = getControllerInfo(0);

    ras_log_buffer_info("Controller buttons: 0x%04X", buttons);

    for (int i = 0; i < PLAT_BUTTON_COUNT; i++) {

        if (g_plat_to_app[i] == 0) {
            continue;
        }
        int app_key_code = g_plat_to_app[i];

        RasKeyEvent app_key_state_prior = plat_input_state.keys[app_key_code];
        RasKeyEvent plat_key_state = (buttons & (1 << i)) ? RAS_KEY_EVENT_DOWN : RAS_KEY_EVENT_NONE;

        bool had_up_event = app_key_state_prior == RAS_KEY_EVENT_DOWN
            && plat_key_state == RAS_KEY_EVENT_NONE;

        plat_input_state.keys[app_key_code] = had_up_event ? RAS_KEY_EVENT_UP : plat_key_state;
    }

    plat_input_state.mods = (buttons & (1 << PLAT_BUTTON_L1))
        ? RAS_KMOD_CTRL
        : RAS_KMOD_NONE;
    plat_input_state.mods = (buttons & (1 << PLAT_BUTTON_L2))
        ? plat_input_state.mods | RAS_KMOD_SHIFT
        : plat_input_state.mods;
}
