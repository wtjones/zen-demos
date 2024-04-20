#include "larse/core/parse.h"
#include "test_support.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>

int test_symbol()
{
    // FIXME
    lar_parse_expression(NULL);
    return 0;
}

int test_string()
{
    return 0;
}

TestFn test_fns[] = {
    { "TEST_STRING", test_string },
    { "TEST_SYMBOL", test_symbol }
};

int main(int argc, const char** argv)
{

    if (argc == 1) {
        fprintf(stderr, "Test name required.\n");
        return 1;
    }

    for (int i = 1; i < argc; ++i) {
        printf("main(): argv[%d]: %s\n", i, argv[i]);
    }
    size_t num_fns = sizeof(test_fns) / sizeof(test_fns[0]);

    TestFn* test_fn = test_fn_lookup(test_fns, num_fns, argv[1]);
    if (test_fn == NULL) {
        fprintf(stderr, "Error: Test %s not found.\n", argv[1]);
        return 1;
    }
    return test_fn->fn();
}
