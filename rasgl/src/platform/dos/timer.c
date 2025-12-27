#include "rasgl/core/timer.h"
#include <allegro.h>

/**
 * @brief Milliseconds since app start
 *
 */
volatile uint32_t timer_count = 0;

/**
 * @brief Platform implementation to get number of ticks (milliseconds) since app start.
 * The return type is explicit due to differences in stdint.h on DOS.
 *
 * @return long unsigned int
 */
long unsigned int ras_timer_get_ticks(void)
{
    return (long unsigned int)timer_count;
}

void timer_handler()
{
    timer_count++;
}
END_OF_FUNCTION(timer_handler)

int ras_dos_timer_init(void)
{
    LOCK_VARIABLE(timer_count);
    LOCK_FUNCTION(timer_handler);
    if (install_timer() != 0) {
        return -1;
    }
    if (install_int_ex(timer_handler, BPS_TO_TIMER(RAS_TIMER_TICKS_PER_SECOND)) != 0) {
        remove_timer();
        return -1;
    }

    return 0;
}
void ras_dos_timer_shutdown(void)
{
    remove_timer();
}
