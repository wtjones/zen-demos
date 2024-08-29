#ifndef TEST_SUPPORT_H
#define TEST_SUPPORT_H

#include "log.c/src/log.h"
#include <stdbool.h>
#include <stddef.h>
#include <string.h>

typedef struct TestFn {
    char* name;
    int (*fn)();
} TestFn;

TestFn* test_fn_lookup(TestFn test_fns[], size_t num_test_fns, const char* fn_name);

#endif
