#include "io.h"
#include <stdio.h>
#include <stdlib.h>

char* io_read_file(const char* path)
{
    FILE* file = fopen(path, "r");

    if (!file) {
        fprintf(stderr, "Can't open file: %s\n", path);
        return NULL;
    }
    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    char* file_buffer = malloc(file_size + 1);
    if (NULL == file_buffer) {
        fprintf(stderr, "Can't malloc()!\n");
        return NULL;
    }

    fread(file_buffer, file_size, 1, file);
    fclose(file);
    file_buffer[file_size] = '\0';
    return file_buffer;
}
