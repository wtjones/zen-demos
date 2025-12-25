#ifndef RAS_TIMER_H
#define RAS_TIMER_H

#include <stdint.h>

#define RAS_TIMER_TICKS_PER_SECOND 1000

/**
 * @brief Get the current tick count in milliseconds since app start.
 * Implementation is platform-specific.
 *
 * @return uint32_t Tick count
 */
uint32_t ras_timer_get_ticks(void);

#endif // RAS_TIMER_H
