#include "log.c/src/log.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/scene.h"
#include "rasgl/hosted/sc_load.h"
#include "rasgl/pack/pack.h"
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
    ras_log_info("Packaging scene from %s to %s\n", scene_path, output_path ? output_path : "default output");

    RasScene* scene;
    RasResult result = core_load_scene(scene_path, &scene);

    RAS_CHECK_RESULT_AND_LOG(result, "Failed to load scene from %s\n", scene_path);

    ras_log_info("Successfully loaded scene: %s\n", scene->name);

    size_t encoded_size;
    char* encoded = pack_encode_scene(scene, &encoded_size);

    if (encoded == NULL) {
        ras_log_error("Failed to encode scene.\n");
        core_free_scene(&scene);
        return 1;
    }

    FILE* output_file = fopen(output_path, "wb");
    if (output_file == NULL) {
        ras_log_error("Failed to open output file %s for writing.\n", output_path);
        free(encoded);
        core_free_scene(&scene);
        return 1;
    }
    fwrite(encoded, 1, encoded_size, output_file);
    fclose(output_file);
    free(encoded);
    core_free_scene(&scene);
    ras_log_info("Scene packaged successfully to %s with size %zu bytes",
        output_path, encoded_size);
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
