#ifndef STRING_H
#define STRING_H

#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef intptr_t ssize_t;

/**
 * getline fallback for non-posix
 *
 */
ssize_t core_getline(char** lineptr, size_t* n, FILE* stream);

#endif
