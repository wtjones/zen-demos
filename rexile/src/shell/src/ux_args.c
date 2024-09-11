
#include "ux_args.h"
#include "log.c/src/log.h"
#include <getopt.h>
#include <stdio.h>

void print_usage()
{
    printf("Rexile is a A solitaire sometimes known as *Kings in the Corners Solitaire*. \n"
           "Usage: rexile [options]\n"
           "\n"
           "Informative output:\n"
           " -h, --help    - print this help and exit\n"
           " -v            - verbose output\n"
           "Actions:\n"
           " -d file - provide a draw deck\n");
}

bool ux_parse_options(int argc, char* argv[], UXOptions* options)
{
    options->initial_deck_file = NULL;
    options->verbose = false;

    int c;
    while ((c = getopt(argc, argv, "vd:")) != -1) {
        switch (c) {
        case 'v':
            options->verbose = true;
            log_set_level(LOG_TRACE);
            break;
        case 'd':
            options->initial_deck_file = optarg;
            break;
        case '?':
            if (optopt == 'd') {
                log_error("Option -%c requires an argument.", optopt);
            }
            print_usage();
            return false;
        default:
            abort();
        }
    }
    return true;
}
