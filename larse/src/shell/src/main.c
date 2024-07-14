#include "larse/core/expression.h"
#include "larse/core/logging.h"
#include "larse/core/parse.h"
#include "larse/core/repr.h"
#include "log.c/src/log.h"
#include <stdio.h>
#include <stdlib.h>

void print_usage()
{
    printf("Larse is a configuration language based on lisp\n"
           "Usage: larse [options] [scriptfile]\n"
           " When 'scriptfile' is given, it is loaded.\n"
           "Informative output:\n"
           " -h, --help    - print this help and exit\n"
           "Actions:\n"
           " -x expressions - execute the expressions, then exit\n");
}

int main(int argc, char* argv[])
{
    FILE* log_file = fopen("/tmp/larse.log", "w");
    lar_log_configure(log_file);

    if (argc >= 3 && strcmp(argv[1], "-x") == 0) {

        const char* script_raw = argv[2];
        LarScript* script;
        LarParseResult result = lar_parse_script(script_raw, &script);

        if (result != LAR_PARSE_RESULT_OK) {
            log_error("Error parsing script", "");
            return 1;
        }

        char* repr = lar_repr_script(script);
        log_info("Shell output: \n%s", repr);
        printf("%s\n", repr);
        free(repr);
        lar_free_script(&script);
        return 0;
    }

    if (argc == 2) {
        const char* script_path = argv[1];
        LarScript* script;
        LarParseResult result = lar_parse_file(script_path, &script);

        if (result != LAR_PARSE_RESULT_OK) {
            log_error("Error parsing script", "");
            return 1;
        }
        char* repr = lar_repr_script(script);
        log_info("Shell output: \n%s", repr);
        printf("%s\n", repr);
        free(repr);
        lar_free_script(&script);
        return 0;
    }

    print_usage();

    return 0;
}
