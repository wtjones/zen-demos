#include "larse/core/expression.h"
#include "larse/core/logging.h"
#include "larse/core/merge.h"
#include "larse/core/parse.h"
#include "larse/core/repr.h"
#include "larse/core/version.h"
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
    printf("Larse %s is a configuration language based on lisp\n"
           "Usage: larse [options] [scriptfile]\n"
           " When 'scriptfile' is given, it is loaded.\n"
           "Informative output:\n"
           " -h, --help    - print this help and exit\n"
           " -d            - verbose output\n"
           " -v            - print the version\n"
           "Actions:\n"
           " -x expressions - execute the expressions, then exit\n"
           " -m file-to-merge - apply a 2nd [scriptfile]\n",
        LAR_VERSION_STRING);
}

int handle_parse_result(LarParseResult result, LarScript** script)
{
    if (result != LAR_PARSE_RESULT_OK) {
        log_error("Error parsing script", "");
        return 1;
    }

    char* repr = lar_repr_script(*script);
    log_debug("Shell output: \n%s", repr);
    printf("%s\n", repr);
    free(repr);
    lar_free_script(script);
    return 0;
}

int main(int argc, char** argv)
{
    int vflag = 0;
    bool merge = false;
    char* merge_file = NULL;
    char* expression = NULL;
    int c;

    FILE* log_file = fopen(LAR_DEFAULT_LOG_FILE, "w");
    lar_log_configure(log_file);
    while ((c = getopt(argc, argv, "vm:x:")) != -1) {
        switch (c) {
        case 'd':
            vflag = 1;
            log_set_level(LOG_INFO);
            break;
        case 'v':
            vflag = 1;
            printf("Larse %s\n", LAR_VERSION_STRING);
            return 0;
            break;
        case 'm':
            merge = true;
            merge_file = optarg;
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

    log_debug("vflag = %d, expression = %s, merge = %s\n",
        vflag,
        expression ? expression : "NULL",
        merge_file ? merge_file : "NULL");

    if (expression) {
        LarScript* script;
        return handle_parse_result(
            lar_parse_script(expression, &script), &script);
    }

    if (optind >= argc) {
        print_usage();
        return 0;
    }

    if (merge) {
        LarScript* script0;
        if (lar_parse_file(argv[optind], &script0) != LAR_PARSE_RESULT_OK) {
            return 1;
        }

        LarScript* script1;
        if (lar_parse_file(merge_file, &script1) != LAR_PARSE_RESULT_OK) {
            lar_free_script(&script0);
            return 1;
        }

        LarScript* merge_result = lar_merge_script(script0, script1);
        lar_free_script(&script0);
        lar_free_script(&script1);
        if (!merge_result) {
            return 0;
        }
        return handle_parse_result(LAR_PARSE_RESULT_OK, &merge_result);
        lar_free_script(&merge_result);
    }

    // Default: Parse single file.
    LarScript* script;
    return handle_parse_result(
        lar_parse_file(argv[optind], &script), &script);

    print_usage();

    return 0;
}
