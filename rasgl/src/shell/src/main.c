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
    RasResult result = RAS_RESULT_ERROR;
    RasScene* scene = NULL;
    char* encoded = NULL;
    FILE* output_file = NULL;
    size_t encoded_size = 0;

    ras_log_info("Packaging scene from %s to %s\n",
        scene_path, output_path ? output_path : "default output");

    result = core_load_scene(scene_path, &scene);
    if (result != RAS_RESULT_OK) {
        ras_log_error("Failed to load scene from %s\n", scene_path);
        goto cleanup;
    }

    ras_log_info("Successfully loaded scene: %s\n", scene->name);

    encoded = pack_encode_scene(scene, &encoded_size);
    if (!encoded) {
        ras_log_error("Failed to encode scene.\n");
        goto cleanup;
    }

    if (encoded_size == 0) {
        ras_log_error("Encoded scene has size 0. Aborting.\n");
        goto cleanup;
    }

    output_file = fopen(output_path, "wb");
    if (!output_file) {
        ras_log_error("Failed to open output file %s for writing.\n", output_path);
        goto cleanup;
    }

    size_t written = fwrite(encoded, 1, encoded_size, output_file);
    if (written != encoded_size) {
        ras_log_error("Failed to write entire encoded scene to output file.\n");
        goto cleanup;
    }

    ras_log_info("Scene %s packaged successfully to %s with size %zu bytes",
        scene->name, output_path, encoded_size);

    result = RAS_RESULT_OK;

cleanup:
    if (output_file) {
        fclose(output_file);
    }
    free(encoded);
    if (scene) {
        core_free_scene(&scene);
    }
    return result;
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
