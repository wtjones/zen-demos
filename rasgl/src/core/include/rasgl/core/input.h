#ifndef INPUT_H
#define INPUT_H

#define RAS_KEY_UP 0
#define RAS_KEY_DOWN 1
#define RAS_KEY_LEFT 2
#define RAS_KEY_RIGHT 3
#define RAS_KEY_W 4
#define RAS_KEY_A 5
#define RAS_KEY_S 6
#define RAS_KEY_D 7
#define RAS_KEY_Q 8
#define RAS_MAX_KEYS 9

typedef struct InputState {
    uint8_t keys[RAS_MAX_KEYS];
} InputState;

#endif
