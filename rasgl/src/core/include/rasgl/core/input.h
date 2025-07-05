#ifndef INPUT_H
#define INPUT_H

#include <stdint.h>

/**
 * @brief A single character is expected, however SDL2's event.text.text
 * capacity is used as a baseline. Multi-byte unicode support is not planned,
 * so the platform code could instead ensure a single char is captured.
 *
 */
#define RAS_INPUT_STATE_TEXT_CAPACITY 32

typedef enum {
    RAS_KEY_UNKNOWN,
    RAS_KEY_A,
    RAS_KEY_B,
    RAS_KEY_C,
    RAS_KEY_D,
    RAS_KEY_E,
    RAS_KEY_F,
    RAS_KEY_G,
    RAS_KEY_H,
    RAS_KEY_I,
    RAS_KEY_J,
    RAS_KEY_K,
    RAS_KEY_L,
    RAS_KEY_M,
    RAS_KEY_N,
    RAS_KEY_O,
    RAS_KEY_P,
    RAS_KEY_Q,
    RAS_KEY_R,
    RAS_KEY_S,
    RAS_KEY_T,
    RAS_KEY_U,
    RAS_KEY_V,
    RAS_KEY_W,
    RAS_KEY_X,
    RAS_KEY_Y,
    RAS_KEY_Z,
    RAS_KEY_LCTRL,
    RAS_KEY_RCTRL,
    RAS_KEY_LSHIFT,
    RAS_KEY_RSHIFT,
    RAS_KEY_MINUS,
    RAS_KEY_EQUALS,
    RAS_KEY_TAB,
    RAS_KEY_ESCAPE,
    RAS_KEY_LEFTBRACKET,
    RAS_KEY_RIGHTBRACKET,
    RAS_KEY_LEFTBRACE,
    RAS_KEY_RIGHTBRACE,
    RAS_KEY_UP,
    RAS_KEY_DOWN,
    RAS_KEY_LEFT,
    RAS_KEY_RIGHT,
    RAS_KEY_F1,
    RAS_KEY_F2,
    RAS_KEY_F3,
    RAS_KEY_F4,
    RAS_KEY_F5,
    RAS_KEY_F6,
    RAS_KEY_F7,
    RAS_KEY_F8,
    RAS_KEY_F9,
    RAS_KEY_F10,
    RAS_KEY_F11,
    RAS_KEY_F12,
    RAS_KEY_BACKQUOTE,
    RAS_KEY_RETURN,
    RAS_KEY_BACKSPACE,
    RAS_KEY_SPACE,
    RAS_KEY_COUNT
} RasKey;

typedef enum {
    RAS_KEY_EVENT_NONE,
    RAS_KEY_EVENT_DOWN,
    RAS_KEY_EVENT_UP
} RasKeyEvent;

typedef struct InputState {
    RasKeyEvent keys[RAS_KEY_COUNT];
    uint32_t current_frame;
    char text[RAS_INPUT_STATE_TEXT_CAPACITY];
} InputState;

void core_input_init(InputState* state);

#endif
