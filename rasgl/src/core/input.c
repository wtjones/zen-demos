#include "rasgl/core/input.h"

void core_input_init(InputState* state)
{
    for (int i = 0; i < RAS_MAX_KEYS; i++) {
        state->keys[i] = RAS_KEY_EVENT_NONE;
    }
}
