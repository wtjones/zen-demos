#include "larse/core/expression.h"
#include "larse/core/logging.h"
#include "larse/core/parse.h"
#include "larse/core/repr.h"
#include "log.c/src/log.h"
#ifndef __MSDOS__
#    include <getopt.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#ifdef __MSDOS__
#    define LAR_DEFAULT_LOG_FILE "L:\\LARSE.LOG"
#else
#    define LAR_DEFAULT_LOG_FILE "/tmp/larse.log"
#endif

void print_usage()
{
    printf("Larse is a configuration language based on lisp\n"
           "Usage: larse [options] [scriptfile]\n"
           " When 'scriptfile' is given, it is loaded.\n"
           "Informative output:\n"
           " -h, --help    - print this help and exit\n"
           " -v            - verbose output\n"
           "Actions:\n"
           " -x expressions - execute the expressions, then exit\n");
}

int handle_parse_result(LarParseResult result, LarScript** script)
{
    if (result != LAR_PARSE_RESULT_OK) {
        log_error("Error parsing script", "");
        return 1;
    }

    char* repr = lar_repr_script(*script);
    log_info("Shell output: \n%s", repr);
    printf("%s\n", repr);
    free(repr);
    lar_free_script(script);
    return 0;
}

int main(int argc, char** argv)
{
    int vflag = 0;
    char* expression = NULL;
    int c;

    FILE* log_file = fopen(LAR_DEFAULT_LOG_FILE, "w");
    lar_log_configure(log_file);
    while ((c = getopt(argc, argv, "vx:")) != -1) {
        switch (c) {
        case 'v':
            vflag = 1;
            log_set_level(LOG_INFO);
            break;
        case 'x':
            expression = optarg;
            break;
        case '?':
            if (optopt == 'x') {
                log_error("Option -%c requires an argument.", optopt);
            }
            print_usage();
            return 1;
        default:
            abort();
        }
    }

    log_info("vflag = %d, expression = %s\n", vflag, expression ? expression : "NULL");

    if (expression) {
        LarScript* script;
        return handle_parse_result(
            lar_parse_script(expression, &script), &script);
    }
    if (optind < argc) {
        log_info("script file argument: %s\n", argv[optind]);
        LarScript* script;
        return handle_parse_result(
            lar_parse_file(argv[optind], &script), &script);
    }

    print_usage();

    return 0;
}
