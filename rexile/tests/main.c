
#include "test_support.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>

int test_empty_deck_loses()
{
    // Arrange
    return false;
}

int test_invalid_placement_returns_invald()
{
    // Arrange
    return true;
}

TestFn test_fns[] = {
    { "test_empty_deck_loses", test_empty_deck_loses },
    { "test_invalid_placement_returns_invald", test_invalid_placement_returns_invald }
};

int main(int argc, const char** argv)
{
    if (argc == 1) {
        fprintf(stderr, "Test name required.\n");
        return 1;
    }

    size_t num_fns = sizeof(test_fns) / sizeof(test_fns[0]);

    TestFn* test_fn = test_fn_lookup(test_fns, num_fns, argv[1]);
    if (test_fn == NULL) {
        fprintf(stderr, "Error: Test %s not found.\n", argv[1]);
        return 1;
    }
    return test_fn->fn();
}
