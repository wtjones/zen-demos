#ifndef INPUT_H
#define INPUT_H

#include <stdint.h>

#define RAS_KEY_UP 0
#define RAS_KEY_DOWN 1
#define RAS_KEY_LEFT 2
#define RAS_KEY_RIGHT 3
#define RAS_KEY_W 4
#define RAS_KEY_A 5
#define RAS_KEY_S 6
#define RAS_KEY_D 7
#define RAS_KEY_Q 8
#define RAS_KEY_EQUALS 9
#define RAS_KEY_MINUS 10
#define RAS_KEY_E 11
#define RAS_KEY_ESCAPE 12
#define RAS_KEY_TAB 13
#define RAS_KEY_P 14
#define RAS_KEY_LEFTBRACKET 15
#define RAS_KEY_RIGHTBRACKET 16
#define RAS_KEY_B 17

#define RAS_MAX_KEYS 255

typedef enum {
    RAS_KEY_EVENT_NONE,
    RAS_KEY_EVENT_DOWN,
    RAS_KEY_EVENT_UP
} RasKeyEvent;

typedef struct InputState {
    RasKeyEvent keys[RAS_MAX_KEYS];
    uint32_t current_frame;
} InputState;

void core_input_init(InputState* state);

#endif
