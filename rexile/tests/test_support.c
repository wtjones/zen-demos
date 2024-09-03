#include "test_support.h"
#include <stdio.h>

TestFn* test_fn_lookup(TestFn test_fns[], size_t num_test_fns, const char* fn_name)
{
    if (!fn_name) {
        fprintf(stderr, "Test name not provided\n");
        return NULL;
    }

    for (size_t i = 0; i < num_test_fns; ++i) {
        log_trace("comparing input to fn name %s\n", test_fns[i].name);
        if (!strcmp(fn_name, test_fns[i].name)) {
            return &test_fns[i];
        }
    }

    fprintf(stderr, "Test %s not found.\n", fn_name);
    return NULL;
}
