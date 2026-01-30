#include "log.c/src/log.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/event.h"

void ras_log_init(void)
{
    core_event_summary_init();
    g_ras_log_fn = log_log_ex;
}
