#include "log.c/src/log.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/event.h"
#include "rasgl/hosted/event_log.h"
#include <string.h>

void ras_log_init(void)
{
    core_event_summary_init();
    hosted_event_summary_log_init();
    g_ras_log_fn = log_log_ex;
}
