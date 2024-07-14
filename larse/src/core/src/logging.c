#include "larse/core/logging.h"
#include "log.c/src/log.h"

void lar_log_configure(FILE* log_file)
{
    log_add_fp(log_file, LOG_INFO);
    log_set_quiet(0);
    log_set_level(LOG_ERROR);
}
