#include "log.c/src/log.h"
#include "rasgl/core/scene.h"
#include "rasgl/hosted/sc_load.h"
#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>

void print_usage()
{
    printf("rascli is a command-line tool for rasgl\n"
           "Usage: rascli [options]\n"
           "Informative output:\n"
           " -h, --help             - print this help and exit\n"
           "Actions:\n"
           " -p, --package <path>   - package a scene file\n"
           " -o, --output <path>    - specify output path for the packaged scene\n");
}

int handle_package_scene(const char* scene_path, const char* output_path)
{
    printf("Packaging scene from %s to %s\n", scene_path, output_path ? output_path : "default output");

    RasScene* scene;
    RasResult result = core_load_scene(scene_path, &scene);

    if (result != RAS_RESULT_OK) {
        fprintf(stderr, "Failed to load scene from %s\n", scene_path);
        return 1;
    }
    printf("Successfully loaded scene: %s\n", scene->name);
    return 0;
}

int main(int argc, char** argv)
{
    char* scene_path = NULL;
    char* output_path = NULL;
    int c;

    FILE* log_file = fopen("/tmp/rasgl.log", "w");

    log_add_fp(log_file, RAS_LOG_LEVEL_FILE);
    log_set_level(LOG_INFO);
    log_set_quiet(false);
    ras_log_init();

    static struct option long_options[] = {
        { "help", no_argument, 0, 'h' },
        { "package", required_argument, 0, 'p' },
        { "output", required_argument, 0, 'o' },
        { 0, 0, 0, 0 }
    };

    while ((c = getopt_long(argc, argv, "hp:o:", long_options, NULL)) != -1) {
        switch (c) {
        case 'h':
            print_usage();
            return 0;
        case 'p':
            scene_path = optarg;
            break;
        case 'o':
            output_path = optarg;
            break;
        case '?':
            print_usage();
            return 1;
        default:
            abort();
        }
    }

    if (scene_path && output_path) {
        return handle_package_scene(scene_path, output_path);
    }

    print_usage();
    return 0;
}
