#ifndef HOSTED_EVENT_H
#define HOSTED_EVENT_H

#include "log.c/src/log.h"
#include "rasgl/core/event.h"
#include <stddef.h>
#include <stdint.h>

/**
 * @brief Wire up callback with log.c library.
 *
 */
void hosted_event_summary_log_init();
/**
 * @brief Callback for log.c to capture the most recent event by category.
 *
 * @param log_event
 */
void hosted_event_summary_cb(log_Event* log_event);

#endif
