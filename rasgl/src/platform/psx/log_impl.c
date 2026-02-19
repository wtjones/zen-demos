#include "rasgl/core/debug.h"
#include "rasgl/core/event.h"
#include "serial.h"
#include <stdio.h>
#include <string.h>

static const char* repr_level[] = {
    "TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"
};

static void platform_log(
    int level,
    int category,
    const char* file,
    int line,
    const char* fmt,
    va_list args)
{
    (void)level;
    (void)category;
    (void)file;
    (void)line;
    (void)fmt;
    (void)args;

    if (level < RAS_LOG_LEVEL_FILE) {
        return;
    }

    static char buffer[1000];
    static char buffer2[1000];

    va_list args_copy;
    va_copy(args_copy, args);
    vsnprintf(buffer, sizeof buffer, fmt, args_copy);
    va_end(args_copy);

    snprintf(
        buffer2,
        sizeof buffer2,
        "%s %s:%d: %s\n",
        repr_level[level],
        file,
        line,
        buffer);

    for (int i = 0; i < strlen(buffer2); i++)
        print_char(buffer2[i]);
}

void ras_log_init(void)
{
    core_event_summary_init();
    g_ras_log_fn = platform_log;
}
